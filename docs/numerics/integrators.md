# Integrators

## Runge-Kutta

The generic function `RK4` in package `:::ada Orka.Integrators` can be
used to perform numerical integration using the Runge-Kutta 4th order method.

It returns the change to a value for a given time step.
If the parameter `F` represent a time-variant system, that is, `F` depends on a time `T`,
then you must compute the derivative at time `:::ada T + DT`. In this case you
must keep track of `T` yourself and add it to the given `DT`.

The returned value must be added to Y to perform the numerical integration.

For example, to numerically compute the position and velocity of an object
using the momentum and the inverse of the mass:

```ada
type State is record
   Position, Momentum, Velocity : Vectors.Vector4;
   Inverse_Mass                 : Orka.Float_64;
end record;

type Derivative is record
   DX, DP : Vectors.Vector4 := Vectors.Vector4 (Vectors.Zero_Point);
end record;
```

where `DX` is the derivative of the position (velocity) and `DP` is the
derivative of the momentum (impulse).

!!! note "Momentum **p** = *m* **v** where *m* is the mass and **v** the velocity"

A few functions must then be defined to scale  `Derivative` and add a
`Derivative` to a `State` or another `Derivative`:

```ada
function "+" (S : State; Motion : Derivative) return State is
   Result : State := S;
begin
   Result.Position := Result.Position + Motion.DX;
   Result.Momentum := Result.Momentum + Motion.DP;

   --  Recompute velocity after updating momentum
   Result.Velocity := Result.Momentum * Result.Inverse_Mass;

   return Result;
end "+";

function "*" (Left : Orka.Float_64; Right : Derivative) return Derivative is
  ((DX => Left * Right.DX, DP => Left * Right.DP));

function "+" (Left, Right : Derivative) return Derivative is
  ((DX => Left.DX + Right.DX, DP => Left.DP + Right.DP));
```

These functions are then used to instantiate the generic function `RK4`
and to perform the numerical integration:

```ada
function RK4 is new Orka.Integrators.RK4 (State, Derivative, Orka.Float_64);

procedure Integrate
  (S     : in out State;
   Force : not null access function (S : State; Time : Orka.Float_64) return Vectors.Vector4;
   T, DT : Orka.Float_64)
is
   function F (Y : State; DT : Orka.Float_64) return Derivative is
     ((DX => Y.Velocity, DP => Force (Y, T + DT)));
begin
   S := S + RK4 (S, DT, F'Access);
end Integrate;
```
