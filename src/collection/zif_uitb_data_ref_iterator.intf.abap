INTERFACE ZIF_UITB_data_ref_iterator
  PUBLIC .
  METHODS has_next
    RETURNING
      VALUE(rf_has_next) TYPE abap_bool.
  METHODS get_next
    RETURNING
      VALUE(rr_element) TYPE REF TO data.
ENDINTERFACE.
