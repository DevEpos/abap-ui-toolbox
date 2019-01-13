INTERFACE zif_uitb_data_ref_list
  PUBLIC .


  METHODS add
    IMPORTING
      !ir_s_element         TYPE REF TO data
    RETURNING
      VALUE(rr_new_element) TYPE REF TO data .
  METHODS add_list DEFAULT IGNORE
    IMPORTING
      !ir_t_list TYPE REF TO data .
  METHODS create_new_line DEFAULT IGNORE
    RETURNING
      VALUE(rr_new_element) TYPE REF TO data .
  METHODS clear .
  METHODS get_all
    RETURNING
      VALUE(rr_t_data) TYPE REF TO data .
  METHODS size
    RETURNING
      VALUE(rv_size) TYPE sy-tabix .
  METHODS get_iterator
    IMPORTING
      !iv_where          TYPE string OPTIONAL
    RETURNING
      VALUE(rr_iterator) TYPE REF TO zif_uitb_data_ref_iterator .
  METHODS get_element
    IMPORTING
      !iv_index          TYPE sy-tabix
      !iv_where          TYPE string OPTIONAL
    EXPORTING
      !ev_index_of_found TYPE sy-tabix
    RETURNING
      VALUE(rr_element)  TYPE REF TO data
    RAISING
      zcx_uitb_element_not_found .
  METHODS remove_at DEFAULT IGNORE
    IMPORTING
      !iv_index TYPE sy-tabix
    RAISING
      zcx_uitb_element_not_found .
  METHODS has_component
    IMPORTING
      !iv_fieldname    TYPE fieldname
    RETURNING
      VALUE(rf_exists) TYPE abap_bool .
  "! <p class="shorttext synchronized" lang="en">Checks if list is empty</p>
  "!
  "! @parameter rf_is_empty | <p class="shorttext synchronized" lang="en"></p>
  METHODS is_empty DEFAULT IGNORE
    RETURNING
      VALUE(rf_is_empty) TYPE abap_bool.
ENDINTERFACE.
