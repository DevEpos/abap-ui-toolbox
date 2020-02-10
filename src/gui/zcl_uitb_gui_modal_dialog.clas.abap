"! <p class="shorttext synchronized" lang="en">Modal dialog with optional toolbar</p>
CLASS zcl_uitb_gui_modal_dialog DEFINITION
  PUBLIC
  INHERITING FROM zcl_uitb_gui_screen_base
  ABSTRACT
  CREATE PUBLIC.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized" lang="en">Constructor</p>
    "!
    METHODS constructor
      IMPORTING
        iv_title TYPE string
        if_no_modal_dialog type abap_bool optional.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_uitb_gui_modal_dialog IMPLEMENTATION.

  METHOD constructor.

    super->constructor(
        iv_title    = iv_title
        iv_lifetime = cl_gui_control=>lifetime_dynpro
    ).
    mf_as_dialog = abap_true.
    mf_no_modal_dialog = if_no_modal_dialog.

  ENDMETHOD.

ENDCLASS.
