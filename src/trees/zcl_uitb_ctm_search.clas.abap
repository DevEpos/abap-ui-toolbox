class ZCL_UITB_CTM_SEARCH definition
  public
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IR_MODEL type ref to CL_COLUMN_TREE_MODEL .
  methods FIND
    returning
      value(RS_RESULT) type TREEMIKEY .
  methods FIND_NEXT
    returning
      value(RS_RESULT) type TREEMIKEY .
protected section.
private section.

  data MR_MODEL type ref to CL_COLUMN_TREE_MODEL .
ENDCLASS.



CLASS ZCL_UITB_CTM_SEARCH IMPLEMENTATION.


  METHOD constructor.
    mr_model = ir_model.
  ENDMETHOD.


  METHOD find.
    mr_model->find(
      IMPORTING
        result_type           = DATA(lv_result_type)    " Search result
        result_item_key_table = DATA(lt_result_item_key)    " Search result
    ).

    IF lv_result_type = cl_item_tree_model=>find_match.
      rs_result = lt_result_item_key[ 1 ].
    ELSEIF lv_result_type = cl_item_tree_model=>find_no_match.
      CLEAR rs_result.
    ENDIF.
  ENDMETHOD.


  METHOD find_next.
    mr_model->find_next(
      IMPORTING
        result_type              = DATA(lv_result_type)
        result_item_key_table    = DATA(lt_result_item_key)
*        result_expander_node_key =     " Search result
    ).

    IF lv_result_type = cl_item_tree_model=>find_match.
      rs_result = lt_result_item_key[ 1 ].
    ELSEIF lv_result_type = cl_item_tree_model=>find_no_match.
      CLEAR rs_result.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
