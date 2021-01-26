INTERFACE zif_uitb_gui_command
  PUBLIC .
  "! <p class="shorttext synchronized" lang="en">Type of command</p>
  TYPES ty_command_type TYPE i.

  CONSTANTS:
    BEGIN OF c_command_type,
      request_menu TYPE ty_command_type VALUE 0,
      normal       TYPE ty_command_type VALUE 1,
      dynpro       TYPE ty_command_type VALUE 2,
    END OF c_command_type.

  DATA mv_function TYPE ui_func READ-ONLY.
  DATA mr_params TYPE REF TO data READ-ONLY.
  DATA mv_type TYPE ty_command_type READ-ONLY.

  "! <p class="shorttext synchronized" lang="en">Adds the given menu to the command</p>
  "!
  METHODS set_context_menu
    IMPORTING
      io_menu TYPE REF TO cl_ctmenu.
ENDINTERFACE.
