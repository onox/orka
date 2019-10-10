# Parallel and asynchronous programming

Modern CPUs have multiple cores and to maximize hardware utilization various
parallel programming techniques should be employed. Besides Single Instruction
Multiple Data (SIMD), which provides very fine-grained parallelism at the
hardware level, a few techniques can be used to keep the cores of a CPU busy:

- **Fork and join**. Divide some of the work of a frame up into *n* batches.

- **Multiple tasks**. One task for each subsystem.

- **Jobs**. Divide all the work up into small jobs, which are processed by
  a pool of *n* tasks.

In Orka, a job graph processing system provides flexible multitasking by
allowing work to be split into multiple small *jobs* which are then processed
by any available task from a task pool. Jobs can be processed in parallel
(*fork and join*) as well as sequentially.

Jobs can be executed sequentially by setting some jobs as dependencies of
others, forming a job graph. The execution status of a job graph can be
tracked with a future.

## Creating a system

A job graph processing system can be created by instantiating the
`:::ada Orka.Jobs.System` package:

```ada
package Job_System is new Orka.Jobs.System
  (Maximum_Queued_Jobs => 16,
   Maximum_Job_Graphs  => 4);
```

When this generic package gets instantiated, a number of worker tasks
will be created. These tasks will try to dequeue and process jobs from
a shared `Queue`. The number of worker tasks is stored in the constant
`Number_Of_Workers`.

Instantiating the package requires some parameters to be provided:

- `Maximum_Queued_Jobs`: the maximum number of jobs that can be
  enqueued (the capacity of the queue).

- `Maximum_Job_Graphs`: the maximum number of separate job graphs
  that can be enqueued. For each job graph, a `Future` object is acquired.
  The number of job graphs that can be processed concurrently is bounded
  by this number.

Jobs can be enqueued by calling `Queue.Enqueue`. The worker tasks can be
shut down by calling the procedure `Shutdown`.

## Jobs

A job is some code that performs some work based on some data. It is an
instance of the limited interface `Job`. When a worker picks up a job,
it calls the procedure `Execute` of the job.

A job can have zero or one dependent job (thus the job is a dependency
of the other job). After a job has been executed by a worker, the worker
will decrement the number of dependencies in its dependent job (if there
is one). If the number of dependencies of that other job gets reduced to
zero, then that job will be enqueued to the shared queue so that it can
be processed later by a worker.

The dependent job of a job can be retrieved with the function `Dependent`.
If a job has no dependent job, then a pointer to the `Null_Job` is returned.
The function `Has_Dependencies` returns `True` or `False` depending on
whether it is the dependent of another job.

### Creating a job

A job should inherit from `Abstract_Job`, which implements the interface `Job`:

```ada
type Example_Job is new Orka.Jobs.Abstract_Job with record
   ...
end record;

overriding
procedure Execute
  (Object  : Example_Job;
   Enqueue : not null access procedure (Element : Orka.Jobs.Job_Ptr));
```

Create an instance of the job and assign it to a variable of the type
`Job_Ptr`:

```ada
Job_1 : Orka.Jobs.Job_Ptr := new Example_Job;
```

`Job_1` can then be enqueued via the entry `Queue.Enqueue`.
When the job is executed, it can optionally enqueue new jobs. These jobs
will be enqueued and executed before any dependent job of the job.

### Graphs

Jobs can be connected and form a graph by setting some of its jobs
as dependencies of other jobs. To create a chain of jobs such that each job
is a dependency of the next job, call procedure `Chain`:

```ada
Orka.Jobs.Chain ((Job_1, Job_2, Job_3));
```

This will create the graph `Job_1` --> `Job_2` --> `Job_3`.
Afterwards, only `Job_1` needs to be enqueued manually; the workers will
follow the edges of the graph and enqueue the remaining jobs.

There are some alternative ways to set dependencies. To set one job as the
dependency of another, call `Set_Dependency`:

```ada
Job_2.Set_Dependency (Job_1);
```

This will create the graph `Job_1` --> `Job_2`.

To set multiple jobs as dependencies, use procedure `Set_Dependencies`:

```ada
Job_3.Set_Dependencies ((Job_1, Job_2));
```

This will create a graph with the edges `Job_1` --> `Job_3` and
`Job_2` --> `Job_3`.

!!! info "Dependencies and dependents"

    If a job graph has an edge `Job_1` --> `Job_2`, then `Job_1` is a
    dependency of `Job_2` and `Job_2` is the dependent job of `Job_1`.

    A job can have 0 or 1 dependents and 0 to *n* dependencies.

### Enqueing jobs

In order to execute a job graph by the system, the jobs in the graph
which have no dependencies need to be enqueued via `Queue.Enqueue`. Jobs
are considered to be in the same graph if they share a smart pointer to
an instance of the synchronized interface `Future`. If a smart point is
null, it is set by `Queue.Enqueue`:

```ada
declare
   Future_Pointer : Orka.Futures.Pointers.Mutable_Pointer;
   --  Future_Pointer.Is_Null is True
begin
   Job_System.Queue.Enqueue (Job_1, Future_Pointer);
   --  Future_Pointer.Is_Null is False

   Job_System.Queue.Enqueue (Job_2, Future_Pointer);
   --  Future_Pointer.Is_Null is False
end;
```

Note that `Job_1` and `Job_2` must have the same root dependent job (the
last job of the graph that gets executed); they must be part of one
connected graph. Otherwise the graph will be considered complete as soon as
one of its job has no dependents, which will cause the remaining jobs to
skip execution.

To get the current status of a future, call function `Current_Status`:

```ada
if Future_Pointer.Get.Current_Status = Orka.Futures.Done then
   --  All jobs in the job graph have been executed
end if;
```

#### Awaiting completion

Sometimes you need to wait until a job graph has finished before
continuing. The entry `Wait_Until_Done` can be used to block until the
status becomes `Done` or `Failed`:

```ada
declare
   Status : Orka.Futures.Status;
   Future : constant Orka.Futures.Future_Access := Future_Pointer.Get.Value;
begin
   select
      Future.Wait_Until_Done (Status);

      --  Status is Done
   or
      delay until Clock + Milliseconds (10);

      --  Execution of the graph did not complete within 10 ms
      --  Current status is Future.Current_Status
   end select;
end;
```

If one of the jobs in the graph raises an exception, `Status` becomes
`Failed` and `Wait_Until_Done` will reraise this exception.

### Parallel jobs

So far we have seen how jobs can be created and be made part of a job graph.
Each of these jobs will operate on some data. Because each job is executed
by a single worker, the data is processed sequentially in the job. However,
the amount of data might be big enough that it should be split up so that it
can be processed by multiple workers, each processing a slice of the data.

A job implementing the interface `Parallel_Job` can be made parallelizable
with the function `Parallelize`.
This function will return a pointer to a job which, when it is executed
by a worker, will enqueue multiple instances of the given job that it was
meant to parallelize, each with a different range. These jobs will run in
parallel if there are multiple workers.

Jobs wishing to implement the interface `Parallel_Job` should inherit from
`Abstract_Parallel_Job` and override a different procedure:

```ada
type Example_Parallel_Job is new Jobs.Abstract_Parallel_Job with record
   ...
end record;

overriding
procedure Execute (Object : Example_Parallel_Job; From, To : Positive);
```

An instance of this job can then be created and parallelized with the
function `Parallelize`:

```ada
Job_1 : Jobs.Parallel_Job_Ptr := new Example_Parallel_Job;

Job_2 : Jobs.Job_Ptr := Jobs.Parallelize (Job_1, Length => 24, Slice => 6);
```

When `Job_2` is executed, it will enqueue four instances of `Job_1` with
the ranges 1..6, 7..12, 13..18, and 19..24. These four jobs will be executed
before the dependent job of `Job_2` gets executed (if it has any).

### GPU jobs

Some jobs may use subprograms from packages `:::ada Orka.Rendering` or `GL`,
and thus need to run in a task that holds the OpenGL context. A job can
specify this is the case by implementing the empty interface `GPU_Job`:

```ada
type Render_Scene_Job is new Jobs.Abstract_Job and Jobs.GPU_Job with record
   ...
end record;
```

This interface does not require implementing any subprograms and is simply
used as a marker so that the system knows that the job cannot be processed
by any of the workers (which do not hold the OpenGL context).

In order to process these GPU jobs, the package `:::ada Orka.Jobs.System`
actually provides two queues:

1. **CPU**: Jobs from this queue are dequeued by the workers.

2. **GPU**: Jobs from this queue are not dequeued by the workers, but
   can be dequeued by a special user-defined task.

Jobs are automatically enqueued in the correct queue by `Queue.Enqueue`
depending on whether the job implements the interface `GPU_Job`.
Jobs from the GPU queue must be dequeued and executed in a task that
holds the OpenGL context. In this task, the following code will dequeue
and execute jobs from this queue:

```ada
Job_System.Execute_GPU_Jobs;
```

Note that this procedure blocks until the system is shut down.
