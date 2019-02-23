"! <p class="shorttext synchronized" lang="en">PBO Callback for GUI Screen</p>
INTERFACE zif_uitb_gui_pbo_callback
  PUBLIC .

  "! <p class="shorttext synchronized" lang="en">Maps F-Key functions to given user function</p>
  "!
  METHODS map_fkey_functions
    IMPORTING
      it_fkey_map TYPE zif_uitb_ty_gui_screen=>ty_t_fkey_map.
  "! <p class="shorttext synchronized" lang="en">Maps F-Key function to given user function</p>
  "!
  METHODS map_fkey_function
    IMPORTING
      iv_fkey            TYPE ui_func
      iv_mapped_function TYPE ui_func
      iv_text            TYPE gui_text.
  "! <p class="shorttext synchronized" lang="en">Deactivates the given function</p>
  "!
  METHODS deactivate_function
    IMPORTING
      iv_function TYPE ui_func.
  "! <p class="shorttext synchronized" lang="en">Deactivates the given functions</p>
  "!
  METHODS deactivate_functions
    IMPORTING
      it_functions TYPE ui_functions.
  "! <p class="shorttext synchronized" lang="en">Sets the screen title</p>
  "!
  METHODS set_title
    IMPORTING
      iv_title TYPE string.

  "! <p class="shorttext synchronized" lang="en">Manually sets GUI status</p>
  "!
  METHODS set_status
    IMPORTING
      iv_status   TYPE syst_pfkey
      iv_progname TYPE sy-repid OPTIONAL.
  "! <p class="shorttext synchronized" lang="en">Returns 'X' if first screen call</p>
  "!
  METHODS is_first_screen_call
    RETURNING
      VALUE(rf_is_first) TYPE sap_bool.
ENDINTERFACE.
