CLASS zcl_uitb_ctm_selections DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !ir_model          TYPE REF TO cl_column_tree_model
        !ir_nodes          TYPE REF TO zcl_uitb_ctm_nodes
        !if_item_selection TYPE abap_bool
        !iv_selection_mode TYPE i .
    METHODS get_selected_nodes
      RETURNING
        VALUE(result) TYPE zcl_uitb_ctm_node=>ty_t_ctm_node.
    METHODS get_selected_node
      RETURNING
        VALUE(result) TYPE REF TO zcl_uitb_ctm_node.
    METHODS is_single_selection
      RETURNING
        VALUE(result) TYPE abap_bool .
    METHODS has_item_selection
      RETURNING
        VALUE(result) TYPE abap_bool .
    METHODS select_nodes
      IMPORTING
        !it_nodes TYPE treemnotab .
    METHODS unselect_nodes
      IMPORTING
        !it_nodes TYPE treemnotab .
    METHODS unselect_all_nodes .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mr_model TYPE REF TO cl_column_tree_model .
    DATA mv_selection_mode TYPE i .
    DATA mf_item_selection TYPE abap_bool .
    DATA mr_nodes TYPE REF TO zcl_uitb_ctm_nodes .
ENDCLASS.



CLASS zcl_uitb_ctm_selections IMPLEMENTATION.


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
      mr_model->get_selected_node( IMPORTING node_key = lv_selected_node EXCEPTIONS OTHERS = 1 ).
      IF lv_selected_node IS NOT INITIAL.
        result = VALUE #( ( mr_nodes->get_node( lv_selected_node ) ) ).
      ENDIF.
    ELSE.
      mr_model->get_selected_nodes( IMPORTING node_key_table = DATA(lt_selected_nodes) EXCEPTIONS OTHERS = 1 ).
      LOOP AT lt_selected_nodes ASSIGNING FIELD-SYMBOL(<lv_selected_node>).
        result = VALUE #( BASE result ( mr_nodes->get_node( <lv_selected_node> ) ) ).
      ENDLOOP.
    ENDIF.

    " check if there is a selected item of a node
    IF result IS INITIAL AND mf_item_selection = abap_true.
      mr_model->get_selected_item( IMPORTING node_key = lv_selected_node EXCEPTIONS OTHERS = 1 ).
      IF lv_selected_node IS NOT INITIAL.
        result = VALUE #( ( mr_nodes->get_node( lv_selected_node ) ) ).
      ENDIF.
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
