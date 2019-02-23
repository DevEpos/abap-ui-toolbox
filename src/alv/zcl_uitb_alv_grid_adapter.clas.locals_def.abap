INTERFACE if_alv_dialog.
  INTERFACES zif_uitb_gui_screen.

  ALIASES mf_visible
    FOR zif_uitb_gui_screen~mf_visible.
  ALIASES show
    FOR zif_uitb_gui_screen~show.

  DATA mo_gui_alv_grid TYPE REF TO cl_gui_alv_grid.
  DATA mf_modal TYPE abap_bool.
ENDINTERFACE.

CLASS cl_dialog DEFINITION
INHERITING FROM zcl_uitb_gui_dialog.
  PUBLIC SECTION.
    INTERFACES if_alv_dialog.
    ALIASES mo_gui_alv_grid
      FOR if_alv_dialog~mo_gui_alv_grid.
    ALIASES mf_model
      FOR if_alv_dialog~mf_modal.
    METHODS constructor
      IMPORTING
        iv_title        TYPE string
        io_grid_adapter TYPE REF TO zcl_uitb_alv_grid_adapter.
    METHODS zif_uitb_gui_command_handler~execute_command
        REDEFINITION.
  PROTECTED SECTION.
    METHODS create_content
        REDEFINITION.
  PRIVATE SECTION.
    DATA mo_grid_adapter TYPE REF TO zcl_uitb_alv_grid_adapter.
ENDCLASS.

CLASS cl_modal_dialog DEFINITION
INHERITING FROM zcl_uitb_gui_modal_dialog.
  PUBLIC SECTION.
    INTERFACES if_alv_dialog.
    ALIASES mo_gui_alv_grid
      FOR if_alv_dialog~mo_gui_alv_grid.
    ALIASES mf_model
      FOR if_alv_dialog~mf_modal.
    METHODS constructor
      IMPORTING
        iv_title        TYPE string
        io_grid_adapter TYPE REF TO zcl_uitb_alv_grid_adapter.
    METHODS zif_uitb_gui_command_handler~execute_command
        REDEFINITION.
  PROTECTED SECTION.
    METHODS create_content
        REDEFINITION.
    METHODS do_before_dynpro_output
        REDEFINITION.
  PRIVATE SECTION.
    DATA mo_grid_adapter TYPE REF TO zcl_uitb_alv_grid_adapter.
ENDCLASS.
