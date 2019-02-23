CLASS lcl_exit_callback DEFINITION.
  PUBLIC SECTION.
    INTERFACES zif_uitb_exit_callback.
  PRIVATE SECTION.
    DATA: mf_exit_cancelled TYPE sap_bool.
ENDCLASS.

CLASS lcl_local_controller DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS pai.
    CLASS-METHODS pbo.
    CLASS-METHODS exit.
    CLASS-METHODS clean_up.
    CLASS-METHODS leave_screen.
  PRIVATE SECTION.
    CLASS-METHODS clear_functions.
    CLASS-METHODS fill_dynamic_functions.
    CLASS-METHODS should_exit
      CHANGING
        cf_exit TYPE abap_bool.
ENDCLASS.

CLASS lcl_pbo_callback DEFINITION.

  PUBLIC SECTION.
    DATA mt_fkey_map TYPE zif_uitb_ty_gui_screen=>ty_t_fkey_map READ-ONLY.
    DATA mt_function_exclude TYPE STANDARD TABLE OF sy-ucomm READ-ONLY.

    INTERFACES zif_uitb_gui_pbo_callback.
    METHODS constructor
      IMPORTING
        if_first_screen_call TYPE sap_bool.
  PRIVATE SECTION.
    DATA mf_is_first_screen_call TYPE sap_bool.
ENDCLASS.
