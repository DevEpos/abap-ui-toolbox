CLASS ZCL_UITB_list_iterator DEFINITION
  PUBLIC
  CREATE PROTECTED.

  PUBLIC SECTION.
    INTERFACES ZIF_UITB_iterator.
    CLASS-METHODS create
      IMPORTING
        ir_list            TYPE REF TO ZIF_UITB_list
      RETURNING
        VALUE(rr_iterator) TYPE REF TO ZIF_UITB_iterator.
  PROTECTED SECTION.
    METHODS constructor
      IMPORTING
        ir_list TYPE REF TO ZIF_UITB_list.
  PRIVATE SECTION.
    DATA mr_list TYPE REF TO ZIF_UITB_list.
    DATA mv_index TYPE sy-tabix.
    DATA mv_size TYPE sy-tabix.

ENDCLASS.



CLASS ZCL_UITB_LIST_ITERATOR IMPLEMENTATION.


  METHOD constructor.
    mr_list = ir_list.
    mv_size = mr_list->size( ).
  ENDMETHOD.


  METHOD create.
    rr_iterator = new ZCL_UITB_list_iterator( ir_list = ir_list ).
  ENDMETHOD.


  METHOD ZIF_UITB_iterator~get_next.
    ADD 1 TO mv_index.
    rr_element = mr_list->get_element( mv_index ).
  ENDMETHOD.


  METHOD ZIF_UITB_iterator~has_next.
    DATA(lv_index) = mv_index + 1.
    TRY.
        mr_list->get_element( iv_index = lv_index ).
        rf_has_next = abap_true.
      CATCH ZCX_UITB_element_not_found.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
