*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS lcl_simple_ui_command IMPLEMENTATION.

  METHOD zif_uitb_gui_command~set_context_menu.
    mo_menu = io_menu.
  ENDMETHOD.

  METHOD constructor.
    mv_function = iv_function.
    mv_type = iv_type.
  ENDMETHOD.

ENDCLASS.
