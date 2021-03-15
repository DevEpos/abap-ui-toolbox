"! <p class="shorttext synchronized" lang="en">Events for popup_get_values checks</p>
CLASS zcl_uitb_pgv_exit_events DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    "! <p class="shorttext synchronized" lang="en">Event to check field</p>
    CLASS-EVENTS check_field_values
      EXPORTING
        VALUE(eo_fields) TYPE REF TO zif_uitb_pgv_check_evt_fields.

    "! <p class="shorttext synchronized" lang="en">Raises CHECK_VALUE event</p>
    CLASS-METHODS raise_check_values_event
      IMPORTING
        io_fields TYPE REF TO zif_uitb_pgv_check_evt_fields.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_uitb_pgv_exit_events IMPLEMENTATION.

  METHOD raise_check_values_event.
    RAISE EVENT check_field_values
      EXPORTING
        eo_fields = io_fields.
  ENDMETHOD.

ENDCLASS.
