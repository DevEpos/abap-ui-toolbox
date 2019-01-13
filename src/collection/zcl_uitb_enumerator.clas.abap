CLASS ZCL_UITB_enumerator DEFINITION
  PUBLIC
  CREATE PROTECTED.

  PUBLIC SECTION.
    INTERFACES ZIF_UITB_enumerator.
    CLASS-METHODS create
      IMPORTING
        ir_enumerable        TYPE REF TO ZIF_UITB_enumerable
      RETURNING
        VALUE(rr_enumerator) TYPE REF TO ZIF_UITB_enumerator.
  PROTECTED SECTION.
    METHODS constructor
      IMPORTING
        ir_enumerable TYPE REF TO ZIF_UITB_enumerable.
  PRIVATE SECTION.
    DATA mr_enumerable TYPE REF TO ZIF_UITB_enumerable.
    DATA mv_index TYPE sy-tabix.
    DATA mv_size TYPE sy-tabix.

ENDCLASS.



CLASS ZCL_UITB_enumerator IMPLEMENTATION.


  METHOD constructor.
    mr_enumerable = ir_enumerable.
    mv_size = mr_enumerable->size( ).
  ENDMETHOD.


  METHOD create.
    rr_enumerator = NEW ZCL_UITB_enumerator( ir_enumerable = ir_enumerable ).
  ENDMETHOD.


  METHOD ZIF_UITB_enumerator~get_next.
    ADD 1 TO mv_index.
    rr_element = mr_enumerable->get_element( mv_index ).
  ENDMETHOD.


  METHOD ZIF_UITB_enumerator~has_next.
    DATA(lv_index) = mv_index + 1.
    TRY.
        mr_enumerable->get_element( iv_index = lv_index ).
        rf_has_next = abap_true.
      CATCH ZCX_UITB_element_not_found.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
