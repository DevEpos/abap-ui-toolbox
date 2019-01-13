CLASS zcl_uitb_data_ref_iterator DEFINITION
  PUBLIC
  CREATE PROTECTED.

  PUBLIC SECTION.
    INTERFACES zif_uitb_data_ref_iterator.
    CLASS-METHODS create
      IMPORTING
        ir_list            TYPE REF TO zif_uitb_data_ref_list
        iv_where           TYPE string OPTIONAL
      RETURNING
        VALUE(rr_iterator) TYPE REF TO zif_uitb_data_ref_iterator.
  PROTECTED SECTION.
    METHODS constructor
      IMPORTING
        ir_list  TYPE REF TO zif_uitb_data_ref_list
        iv_where TYPE string.
  PRIVATE SECTION.
    DATA mr_list TYPE REF TO zif_uitb_data_ref_list.
    DATA mv_index TYPE sy-tabix.
    DATA mv_where TYPE string.
    DATA mv_size TYPE sy-tabix.

ENDCLASS.



CLASS zcl_uitb_data_ref_iterator IMPLEMENTATION.


  METHOD constructor.
    mr_list = ir_list.
    mv_where = iv_where.
    mv_size = mr_list->size( ).
  ENDMETHOD.


  METHOD create.
    rr_iterator = NEW zcl_uitb_data_ref_iterator( ir_list  = ir_list
                                                   iv_where = iv_where ).
  ENDMETHOD.


  METHOD zif_uitb_data_ref_iterator~get_next.

    IF mv_where IS NOT INITIAL.
      rr_element = mr_list->get_element(
        EXPORTING iv_index          = mv_index + 1
                  iv_where          = mv_where
        IMPORTING ev_index_of_found = mv_index
      ).
*      ADD 1 TO mv_index.
    ELSE.
      ADD 1 TO mv_index.
      rr_element = mr_list->get_element( mv_index ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_uitb_data_ref_iterator~has_next.
    DATA(lv_index) = mv_index + 1.
    TRY.
        mr_list->get_element( iv_index = lv_index
                              iv_where = mv_where ).
        rf_has_next = abap_true.
      CATCH zcx_uitb_element_not_found.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
