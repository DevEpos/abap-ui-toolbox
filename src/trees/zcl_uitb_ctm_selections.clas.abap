class ZCL_UITB_CTM_SELECTIONS definition
  public
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IR_MODEL type ref to CL_COLUMN_TREE_MODEL
      !IR_NODES type ref to ZCL_UITB_CTM_NODES
      !IF_ITEM_SELECTION type ABAP_BOOL
      !IV_SELECTION_MODE type I .
  methods GET_SELECTED_NODES
    returning
      value(RESULT) type ZUITB_CTM_NODE_RT
    raising
      ZCX_UITB_TREE_ERROR .
  methods GET_SELECTED_NODE
    returning
      value(RESULT) type ref to ZCL_UITB_CTM_NODE
    raising
      ZCX_UITB_TREE_ERROR .
  methods IS_SINGLE_SELECTION
    returning
      value(RESULT) type ABAP_BOOL .
  methods HAS_ITEM_SELECTION
    returning
      value(RESULT) type ABAP_BOOL .
  methods SELECT_NODES
    importing
      !IT_NODES type TREEMNOTAB .
  methods UNSELECT_NODES
    importing
      !IT_NODES type TREEMNOTAB .
  methods UNSELECT_ALL_NODES .
  PROTECTED SECTION.
private section.

  data MR_MODEL type ref to CL_COLUMN_TREE_MODEL .
  data MV_SELECTION_MODE type I .
  data MF_ITEM_SELECTION type ABAP_BOOL .
  data MR_NODES type ref to ZCL_UITB_CTM_NODES .
ENDCLASS.



CLASS ZCL_UITB_CTM_SELECTIONS IMPLEMENTATION.


  METHOD constructor.
    mr_model = ir_model.
    mr_nodes = ir_nodes.
    mv_selection_mode = iv_selection_mode.
    mf_item_selection = if_item_selection.
  ENDMETHOD.


  METHOD get_selected_node.
    DATA(lt_selected_nodes) = get_selected_nodes( ).
    result = COND #( WHEN lt_selected_nodes IS NOT INITIAL THEN lt_selected_nodes[ 1 ] ).
  ENDMETHOD.


  METHOD get_selected_nodes.
    DATA: lv_selected_node TYPE tm_nodekey.

    CLEAR result.

    IF mv_selection_mode = zif_uitb_c_tree_selection=>single_selection.
      mr_model->get_selected_node( IMPORTING node_key = lv_selected_node ).
      IF lv_selected_node IS NOT INITIAL.
        result = VALUE #( ( mr_nodes->get_node( lv_selected_node ) ) ).
      ENDIF.
    ELSE.
      mr_model->get_selected_nodes( IMPORTING node_key_table = data(lt_selected_nodes) ).
      LOOP AT lt_selected_nodes ASSIGNING FIELD-SYMBOL(<lv_selected_node>).
        result = VALUE #( base result ( mr_nodes->get_node( <lv_selected_node> ) ) ).
      ENDLOOP.
    ENDIF.

    " check if there is a selected item of a node
    IF result IS INITIAL AND mf_item_selection = abap_true.
      mr_model->get_selected_item( IMPORTING node_key = lv_selected_node ).
      result = VALUE #( ( mr_nodes->get_node( lv_selected_node ) ) ).
    ENDIF.
  ENDMETHOD.


  METHOD has_item_selection.
    result = mf_item_selection.
  ENDMETHOD.


  METHOD is_single_selection.
    result = xsdbool( mv_selection_mode = zif_uitb_c_tree_selection=>single_selection ).
  ENDMETHOD.


  METHOD select_nodes.
    CASE mv_selection_mode.

      WHEN zif_uitb_c_tree_selection=>multiple_selection.
        " unselect previously selected nodes
        mr_model->unselect_all( ).
        mr_model->select_nodes( it_nodes ).

      WHEN zif_uitb_c_tree_selection=>single_selection.
        IF it_nodes IS NOT INITIAL AND lines( it_nodes ) = 1.
          mr_model->set_selected_node( it_nodes[ 1 ] ).
        ENDIF.

    ENDCASE.

  ENDMETHOD.


  METHOD unselect_all_nodes.
    mr_model->unselect_all( ).
  ENDMETHOD.


  METHOD unselect_nodes.
    mr_model->unselect_nodes(
      EXPORTING
        node_key_table               = it_nodes
      EXCEPTIONS
        multiple_node_selection_only = 1
        error_in_node_key_table      = 2
        OTHERS                       = 3
    ).
    IF sy-subrc <> 0.
*     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
