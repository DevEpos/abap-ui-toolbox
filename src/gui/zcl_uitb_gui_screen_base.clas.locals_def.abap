*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section
CLASS cl_ui_command DEFINITION.
  PUBLIC SECTION.
    INTERFACES zif_uitb_gui_command.

    ALIASES ty_command_type
      FOR zif_uitb_gui_command~ty_command_type.
    ALIASES mv_function
      FOR zif_uitb_gui_command~mv_function.
    ALIASES mv_type
      FOR zif_uitb_gui_command~mv_type.

    DATA mo_menu TYPE REF TO cl_ctmenu READ-ONLY.

    METHODS constructor
      IMPORTING
        iv_type     TYPE ty_command_type
        iv_function TYPE ui_func.
ENDCLASS.
