INTERFACE ZIF_UITB_list
  PUBLIC .


  METHODS add
    IMPORTING
      !ir_element TYPE REF TO object .
  METHODS clear .
  METHODS size
    RETURNING
      VALUE(rv_size) TYPE sy-tabix .
  METHODS get_iterator
    RETURNING
      VALUE(rr_iterator) TYPE REF TO ZIF_UITB_iterator .
  METHODS get_element
    IMPORTING
      !iv_index         TYPE sy-tabix
    RETURNING
      VALUE(rr_element) TYPE REF TO object
    RAISING
      ZCX_UITB_element_not_found .
  METHODS remove
    IMPORTING
      !ir_element TYPE REF TO object .
  METHODS remove_at
    IMPORTING
      !iv_index TYPE sy-tabix
    RAISING
      ZCX_UITB_element_not_found.
ENDINTERFACE.
