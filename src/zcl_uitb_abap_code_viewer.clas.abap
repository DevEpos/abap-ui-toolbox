CLASS zcl_uitb_abap_code_viewer DEFINITION
  PUBLIC
  INHERITING FROM zcl_uitb_gui_dialog
  CREATE PRIVATE.

  PUBLIC SECTION.

    CLASS-METHODS show_code
      IMPORTING
        iv_title  TYPE string
        iv_theme  TYPE zuitb_code_viewer_theme DEFAULT zif_uitb_c_code_viewer_themes=>default
        it_code   TYPE string_table OPTIONAL
        iv_code   TYPE string OPTIONAL
        iv_width  TYPE i DEFAULT 1400
        iv_height TYPE i DEFAULT 900.
    METHODS zif_uitb_gui_command_handler~execute_command
        REDEFINITION.
  PROTECTED SECTION.
    METHODS create_content
        REDEFINITION.
  PRIVATE SECTION.
    DATA mv_code TYPE string.
    data mv_theme type zuitb_code_viewer_theme.
    DATA mt_code TYPE string_table.
    DATA mo_code_view TYPE REF TO zcl_uitb_abap_code_view.
ENDCLASS.



CLASS zcl_uitb_abap_code_viewer IMPLEMENTATION.

  METHOD show_code.
    zcl_uitb_screen_util=>show_progress( iv_progress = 1 iv_text = |{ 'Source Code is being prepared...'(002) }| ).

    DATA(lr_code_viewer) = NEW zcl_uitb_abap_code_viewer(
      iv_title = iv_title
    ).
    lr_code_viewer->mt_code = it_code.
    lr_code_viewer->mv_code = iv_code.
    lr_code_viewer->mv_theme = iv_theme.

    lr_code_viewer->show(
      iv_width  = iv_width
      iv_height = iv_height
    ).
  ENDMETHOD.

  METHOD zif_uitb_gui_command_handler~execute_command.
    RETURN.
  ENDMETHOD.

  METHOD create_content.
    mo_code_view = NEW zcl_uitb_abap_code_view(
      io_parent_container = io_container
      iv_theme            = mv_theme
    ).

    mo_code_view->show_document(
        it_code = mt_code
        iv_code = mv_code
    ).
  ENDMETHOD.

ENDCLASS.
