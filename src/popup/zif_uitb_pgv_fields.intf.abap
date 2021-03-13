"! <p class="shorttext synchronized" lang="en">Field in popup_get_values function</p>
INTERFACE zif_uitb_pgv_fields
  PUBLIC .

  types tt_fields type STANDARD TABLE OF sval with empty key.

  "! <p class="shorttext synchronized" lang="en">Returns the fields with the current values</p>
  METHODS get_fields
    returning
      value(rt_fields) type tt_fields.

  "! <p class="shorttext synchronized" lang="en">Sets validation error</p>
  METHODS set_validation_error
    IMPORTING
      is_error TYPE svale.
ENDINTERFACE.
