CLASS zcl_uitb_gui_simple_command DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_uitb_gui_command.

    ALIASES mv_function
      FOR zif_uitb_gui_command~mv_function.
    ALIASES mr_params
      FOR zif_uitb_gui_command~mr_params.
    ALIASES mv_type
      FOR zif_uitb_gui_command~mv_type.
    DATA mo_menu TYPE REF TO cl_ctmenu READ-ONLY.

    METHODS constructor
      IMPORTING
        iv_function TYPE ui_func
        iv_type     TYPE zif_uitb_gui_command=>ty_command_type DEFAULT zif_uitb_gui_command=>c_command_type-normal
        ir_params   TYPE REF TO data OPTIONAL.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_uitb_gui_simple_command IMPLEMENTATION.
  METHOD constructor.
    mv_function = iv_function.
    mv_type = iv_type.
    mr_params = ir_params.
  ENDMETHOD.

  METHOD zif_uitb_gui_command~set_context_menu.
    mo_menu = io_menu.
  ENDMETHOD.


ENDCLASS.
