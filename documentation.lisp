(in-package #:org.shirakumo.trivial-benchmark)

;; toolkit.lisp
(docs:define-docs
  (function print-table
    "Print a table of values

TABLE should be a list of lists in row-major order. Each row must have
the same number of elements.

STREAM is the stream to print the table to.

PADDING is the amount of space to insert to the left and right of
every cell.

FORMAT is the format to print the table in. Can be one of:
  :PRINC   --- justs PRINCs the table instead
  :MINIMAL --- prints the table as small as it can
  :FANCY   --- prints the table with box drawing glyphs"))

;; sampler.lisp
(docs:define-docs
  (variable *default-samplers*
    "Which sampler types to use by default.

See SAMPLER
See WITH-SAMPLING")

  (type sampler
    "Representation of a sample measuring method.

See VARIABLES
See WRAP-MEASUREMENT-FORM
See COMMIT-SAMPLES-FORM
See DEFINE-SAMPLER
See WITH-SAMPLING")

  (function variables
    "Return a list of variable specs the sampler needs.

Each variable spec should be a list of the following elements:
  SYMBOL  --- The symbol the variable is bound to
  DEFAULT --- The default value the variable is initialised to
  TYPE    --- The type to declare the variable as

See WITH-SAMPLING
See SAMPLER (type)")

  (function wrap-measurement-form
    "Wrap FORM in a form to record sampling data.

Returns the new form.
The returned form is emitted into a lexical environment where
variables according to the SAMPLER's VARIBALES specs are bound.

See WITH-SAMPLING
See SAMPLER (type)")

  (function commit-samples-form
    "Emit a form to commit measured samples

Returns a new form.
The returned form is emitted into a lexical environment where
COMMIT-FN is bound to a macro with the following behaviour:

  (commit-fn metric sample ...)

  Each pair of METRIC and SAMPLE are recorded in the timer
  METRIC should be a symbol naming the metric to store the sample
  under, and SAMPLE should be a REAL number to record. The sample is
  coerced to a DOUBLE-FLOAT automatically.

See WITH-SAMPLING
See SAMPLER (type)")

  (function define-sampler
    "Convenience macro to define a new sampler type.

NAME should be the name of the new sampler class.
VARS should be a list of variable specs that the sampler will use to
store data during a sampling step. Each spec can be either a symbol
naming the variable, or a variable spec list.
FORMS should match the following structures:

  (:MEASURE (FORM-VAR) BODY...)
  (:COMMIT (COMMIT-FN-VAR) BODY...)

Which are converted to methods for WRAP-MEASUREMENT-FORM and
COMMIT-SAMPLES-FORM respectively. Every variable in VARS is bound
during the body of either, so that you can conveniently emit it into
the resulting forms.

See VARIABLES
See WRAP-MEASUREMENT-FORM
See COMMIT-SAMPLES-FORM
See SAMPLER (type)
See DEFINE-DELTA-SAMPLER")

  (function define-delta-sampler
    "Convenience macro to define a sampler type with a single delta metric.

NAME can either be the name of the sampler, or a list of the name and
a normalisation factor by which the delta is divided to convert it to
a proper sample point.

SAMPLE-POINT-FORMS should be one or more forms whose ultimate return
value evaluates to some REAL number. The forms are emitted *twice*,
the first time before the execution to be measured, and the second
time after. The sample is computed based on the difference of the two
return values.

See DEFINE-SAMPLER
See SAMPLER (type)"))

;; timer.lisp
(docs:define-docs
  (variable *default-computations*
    "Which computations to make by default.

See COMPUTE")

  (variable *default-metrics*
    "Which metrics to include by default.

If NIL, all sampled metrics are included.

See FORMAT-TIMER-STATS")

  (function compute
    "Compute a derived statistic from a vector of samples.

You may add additional methods to this, though the following
computation types are provided by default:

  :COUNT
  :SAMPLES   --- The number of samples
  :TOTAL     --- The sum of all samples
  :MINIMUM   --- The smallest sample
  :MAXIMUM   --- The biggest sample
  :MEDIAN    --- The median of all samples
  :AVERAGE   --- The average of all samples
  :DEVIATION --- The standard deviation based on the average

See *DEFAULT-COMPUTATIONS*")

  (function report-to
    "Print a report about THING to STREAM.

STREAM may be a stream, T, or NIL, according to FORMAT's output
designators.

COMPUTATIONS may be a list of statistical properties to report on a
vector of samples.

Additional arguments are passed on to PRINT-TABLE

See TIMER (type)
See FORMAT-TIMER-STATS
See PRINT-TABLE")

  (function samples
    "Accesses a sample vector for the given metric.

The returned vector is adjustable and has a fill-pointer.
If no vector for the given metric existed before, it is created for
you.

See TIMER (type)")

  (function metric-types
    "Returns a list of metric types included in the timer.

See TIMER (type)")

  (function report
    "Prints a report on the given thing.

This is a wrapper around REPORT-TO.

See REPORT-TO")

  (type timer
    "Encompasses a set of samples.

A timer can be re-used to sample in individual steps.

See FORMAT-TIMER-STATS
See SAMPLES
See METRIC-TYPES
See REPORT-TO
See RESET
See WITH-SAMPLING
See WITH-TIMING")

  (function format-timer-stats
    "Print a table of the samples contained in the timer.

STREAM should be a stream to print to.
TIMER should be a TIMER instance.
COMPUTATIONS should be the derived statistical properties to compute
METRICS should be the metrics to include. If NIL, uses all metrics the
timer recorded samples for.
Other arguments are passed on to PRINT-TABLE

See TIMER (type)
See PRINT-TABLE
See *DEFAULT-COMPUTATIONS*
see *DEFAULT-METRICS*")

  (function reset
    "Resets the timer and clears all samples.

See TIMER (type)")

  (function with-sampling
    "Records sampling information about FORMS into a timer.

TIMER-FORM should evaluate to a TIMER instance, into which the
sampling data will be committed.

SAMPLERS should be a list of names of SAMPLER types that will measure
the FORMS. If none are given, *DEFAULT-SAMPLERS* are used.

The values of FORMS are returned.

During expansion of this macro each sampler in SAMPLERS is
instantiated and variable bindings according to their VARIABLES specs
are emitted. Then, the FORMS are wrapped according to each sampler's
WRAP-MEASUREMENT-FORM return value. The order here can matter, and the
first sampler specified will be the \"innermost\" wrapper. Finally,
each sampler's COMMIT-SAMPLES-FORM is emitted in order to record the
sampling data into the timer.

See *DEFAULT-SAMPLERS*
See SAMPLER (type)
See TIMER (type)
See VARIABLES
See WRAP-MEASUREMENT-FORM
See COMMIT-SAMPLES-FORM")

  (function with-timing
    "Convenience macro to do a one-off timing.

Creates a timer instance according to TIMER, runs WITH-SAMPLING N
times, and then REPORTs the timer using the additional REPORT-ARGS.

See TIMER (type)
See WITH-SAMPLING
See REPORT"))
