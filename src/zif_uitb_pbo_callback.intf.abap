INTERFACE ZIF_UITB_pbo_callback
  PUBLIC .

  "! Deactivates the given function
  "! @parameter iv_function | the function id to be deactivated
  METHODS deactivate_function
    IMPORTING
      iv_function TYPE ui_func.
  "! Deactivates the given functions
  "! @parameter it_functions | the functions to be deactivated
  METHODS deactivate_functions
    IMPORTING
      it_functions TYPE ui_functions.
  "! Sets the screen title
  "! @parameter iv_title | the new title of the screen
  METHODS set_title
    IMPORTING
      iv_title TYPE string.

  METHODS set_status
    IMPORTING
      iv_status   TYPE syst_pfkey
      iv_progname TYPE sy-repid OPTIONAL.
  "! Returns <em>'X'</em> if first screen call
  "! is still in progress
  "! @parameter rf_is_first | first screen call in progress
  METHODS is_first_screen_call
    RETURNING
      VALUE(rf_is_first) TYPE sap_bool.

  "! Leaves the current screen and
  "! goes back to previous one
  METHODS exit_screen.
ENDINTERFACE.
