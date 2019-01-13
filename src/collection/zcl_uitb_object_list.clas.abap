class ZCL_UITB_OBJECT_LIST definition
  public
  create public.

public section.

  interfaces ZIF_UITB_LIST .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mt_table TYPE STANDARD TABLE OF REF TO object.
ENDCLASS.



CLASS ZCL_UITB_OBJECT_LIST IMPLEMENTATION.


  METHOD ZIF_UITB_LIST~ADD.
    APPEND ir_element TO mt_table.
  ENDMETHOD.


  METHOD ZIF_UITB_LIST~CLEAR.
    CLEAR mt_table.
  ENDMETHOD.


  METHOD ZIF_UITB_list~get_element.
    TRY .
        rr_element = mt_table[ iv_index ].
      CATCH cx_sy_itab_line_not_found.
        RAISE EXCEPTION TYPE ZCX_UITB_element_not_found
          EXPORTING
            textid = ZCX_UITB_element_not_found=>index_access
            index  = iv_index.
    ENDTRY.
  ENDMETHOD.


  METHOD ZIF_UITB_LIST~GET_ITERATOR.
    rr_iterator = ZCL_UITB_list_iterator=>create( ir_list = me ).
  ENDMETHOD.


  METHOD ZIF_UITB_LIST~REMOVE.
    ASSIGN mt_table[ table_line = ir_element ] TO FIELD-SYMBOL(<lr_element>).
    IF sy-subrc = 0.
      DELETE mt_table FROM <lr_element>.
    ENDIF.
  ENDMETHOD.


  METHOD ZIF_UITB_LIST~REMOVE_AT.
    DELETE mt_table INDEX iv_index.
  ENDMETHOD.


  METHOD ZIF_UITB_LIST~SIZE.
    rv_size = lines( mt_table ).
  ENDMETHOD.
ENDCLASS.
