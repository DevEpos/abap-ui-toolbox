INTERFACE ZIF_UITB_enumerable
  PUBLIC .
  METHODS size
    RETURNING
      VALUE(rv_size) TYPE sy-tabix .
  METHODS get_enumerator
    RETURNING
      VALUE(rr_enumerator) TYPE REF TO ZIF_UITB_enumerator.
  METHODS get_element
    IMPORTING
      !iv_index         TYPE sy-tabix
    RETURNING
      VALUE(rr_element) TYPE REF TO object
    RAISING
      ZCX_UITB_element_not_found .
ENDINTERFACE.
