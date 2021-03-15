"! <p class="shorttext synchronized" lang="en">Fields in check event of Input dialog</p>
INTERFACE zif_uitb_pgv_check_evt_fields
  PUBLIC .

  TYPES tt_fields TYPE STANDARD TABLE OF sval WITH EMPTY KEY.

  "! <p class="shorttext synchronized" lang="en">Returns the fields with the current values</p>
  METHODS get_fields
    RETURNING
      VALUE(rt_fields) TYPE tt_fields.

  "! <p class="shorttext synchronized" lang="en">Sets validation error</p>
  METHODS set_validation_error
    IMPORTING
      is_error TYPE svale.
ENDINTERFACE.
