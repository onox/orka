# Timers

Timers can record the duration between rendering commands
on the CPU and the GPU.

!!! info
    The various objects described on this page are declared in
    the package `:::ada Orka.Timers`.

## Creating a timer

To create a timer, call the function `Create_Timer`:

```ada
Timer_1 : Timer := Create_Timer;
```

## Starting and stopping

To start measuring the duration of rendering commands on the GPU,
call the procedure `Start`:

```ada
Timer_1.Start;
```

If the timer is currently waiting for the GPU duration to become
available, then `Start` does not do anything.
After the timer has been started, rendering commands can be issued.

To stop measuring the duration of rendering commands on the GPU,
call `Stop`. If the timer is already waiting for the GPU duration to become
available, then `Stop` does not do anything.

After procedure `Stop` has been called, the next restart of the timer
(by calling `Start` again) will update the GPU duration if the result
has become available by then.

### One shot timers

If the timer is used once (not started and stopped repeatedly),
then procedure `Wait_For_Result` needs to be called to wait for the
result of the GPU duration to become available.
Procedure `Wait_For_Result` will wait and stall the CPU until the
result of the GPU duration is available.

If a timer is started and stopped repeatedly (once per frame),
then the GPU duration will become available a few frames after the
timer was started.

## Querying the durations

To query the duration which the CPU spent while the timer was active,
call the function `CPU_Duration`. It will return a `Duration` value.
To query the duration spent by the GPU, call the function `GPU_Duration`.

Values will be zero if no results are available.
