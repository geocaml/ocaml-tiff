exception Tiff_error of string

let raise_error message =
  raise (Tiff_error message)

let raisef fmt =
  Fmt.kstr raise_error fmt

(*Keeping old functions as is for now*)
let invalid_arg message =
  Stdlib.invalid_arg message

let invalid_argf fmt =
  Fmt.kstr invalid_arg fmt  
  