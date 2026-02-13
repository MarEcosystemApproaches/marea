# Timeseries Data Validity

The validity method for the `ea_data` class ensures that the `data` slot
contains the required `year` and `value` columns. This check is
performed automatically upon object creation with `new()`.

## Arguments

- object:

  An `ea_data` object.

## Value

`TRUE` if the object is valid, otherwise a character string describing
the validation failure.
