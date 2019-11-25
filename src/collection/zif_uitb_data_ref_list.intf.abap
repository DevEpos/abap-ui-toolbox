"! <p class="shorttext synchronized" lang="en">List of Data References</p>
INTERFACE zif_uitb_data_ref_list
  PUBLIC .

  "! <p class="shorttext synchronized" lang="en">Add new entry</p>
  METHODS add
    IMPORTING
      !ir_s_element         TYPE REF TO data
    RETURNING
      VALUE(rr_new_element) TYPE REF TO data .
  "! <p class="shorttext synchronized" lang="en">Add Entries from given list (via corresponding)</p>
  METHODS add_list DEFAULT IGNORE
    IMPORTING
      !ir_t_list TYPE REF TO data .
  "! <p class="shorttext synchronized" lang="en">Creates a new entry in the list and returns reference to it</p>
  METHODS create_new_line DEFAULT IGNORE
    RETURNING
      VALUE(rr_new_element) TYPE REF TO data .
  "! <p class="shorttext synchronized" lang="en">Delete all entries in the list</p>
  METHODS clear .
  "! <p class="shorttext synchronized" lang="en">Retrieve reference to table area</p>
  METHODS get_all
    RETURNING
      VALUE(rr_t_data) TYPE REF TO data .
  "! <p class="shorttext synchronized" lang="en">Count entries</p>
  METHODS size
    RETURNING
      VALUE(rv_size) TYPE sy-tabix .
  "! <p class="shorttext synchronized" lang="en">Retrieve iterator instance</p>
  METHODS get_iterator
    IMPORTING
      !iv_where          TYPE string OPTIONAL
    RETURNING
      VALUE(rr_iterator) TYPE REF TO zif_uitb_data_ref_iterator .
  "! <p class="shorttext synchronized" lang="en">Get element at certain index</p>
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
  "! <p class="shorttext synchronized" lang="en">Remove element at index</p>
  METHODS remove_at DEFAULT IGNORE
    IMPORTING
      !iv_index TYPE sy-tabix
    RAISING
      zcx_uitb_element_not_found .
  "! <p class="shorttext synchronized" lang="en">Checks if component exists in line type</p>
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
