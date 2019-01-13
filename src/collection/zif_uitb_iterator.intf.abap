INTERFACE ZIF_UITB_iterator
  PUBLIC .
  METHODS has_next
    RETURNING
      VALUE(rf_has_next) TYPE boolean.
  METHODS get_next
    RETURNING
      VALUE(rr_element) TYPE REF TO object.
ENDINTERFACE.
