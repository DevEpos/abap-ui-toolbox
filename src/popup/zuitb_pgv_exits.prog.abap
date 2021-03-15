*&---------------------------------------------------------------------*
*& Report zuitb_popup_exits
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zuitb_pgv_exits.

CLASS lcl_pgv_exit_event_fields DEFINITION.

  PUBLIC SECTION.
    INTERFACES zif_uitb_pgv_check_evt_fields.

    METHODS constructor
      IMPORTING
        it_fields TYPE zif_uitb_pgv_popup=>tt_dialog_field.
    METHODS get_error
      RETURNING
        VALUE(rs_error) TYPE svale.
  PRIVATE SECTION.
    DATA mt_fields TYPE zif_uitb_pgv_popup=>tt_dialog_field.
    DATA ms_error TYPE svale.
ENDCLASS.

CLASS lcl_pgv_exit_event_fields IMPLEMENTATION.

  METHOD constructor.
    mt_fields = it_fields.
  ENDMETHOD.

  METHOD zif_uitb_pgv_check_evt_fields~get_fields.
    rt_fields = mt_fields.
  ENDMETHOD.

  METHOD zif_uitb_pgv_check_evt_fields~set_validation_error.
    ms_error = is_error.
  ENDMETHOD.

  METHOD get_error.
    rs_error = ms_error.
  ENDMETHOD.

ENDCLASS.

"! <p class="shorttext synchronized" lang="en">Exit for checking field values</p>
FORM exit_check_fields TABLES   fields STRUCTURE sval
                       CHANGING error  STRUCTURE svale.
  DATA(lo_fields) = NEW lcl_pgv_exit_event_fields( CORRESPONDING #( fields[] ) ).
  zcl_uitb_pgv_exit_events=>raise_check_values_event( lo_fields ).
  error = lo_fields->get_error( ).
ENDFORM.
