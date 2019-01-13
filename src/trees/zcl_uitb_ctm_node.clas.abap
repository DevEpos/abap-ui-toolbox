class ZCL_UITB_CTM_NODE definition
  public
  final
  create public .

public section.

  data MV_NODE_KEY type TM_NODEKEY read-only .

  methods CONSTRUCTOR
    importing
      !IV_NODE_KEY type TM_NODEKEY
      !IS_PROPERTIES type TREEMSNOD optional
      !IR_MODEL type ref to CL_COLUMN_TREE_MODEL
      !IR_NODES type ref to ZCL_UITB_CTM_NODES .
  methods DELETE_CHILDREN
    raising
      ZCX_UITB_TREE_ERROR .
  methods GET_CHILDREN
    returning
      value(RT_CHILDREN) type ZUITB_CTM_NODE_RT
    raising
      ZCX_UITB_TREE_ERROR .
  methods GET_EXPANDED_IMAGE
    returning
      value(RESULT) type TV_IMAGE .
  methods GET_FIRST_CHILD
    returning
      value(RESULT) type ref to ZCL_UITB_CTM_NODE
    raising
      ZCX_UITB_TREE_ERROR .
  methods GET_IMAGE
    returning
      value(RESULT) type TV_IMAGE .
  methods GET_ITEM
    importing
      !IV_ITEM_NAME type TV_ITMNAME
    returning
      value(RR_ITEM) type ref to ZCL_UITB_CTM_ITEM
    raising
      ZCX_UITB_TREE_ERROR .
  methods GET_ITEMS
    returning
      value(RESULT) type ZUITB_CTM_ITEM_RT
    raising
      ZCX_UITB_TREE_ERROR .
  methods GET_LAST_CHILD
    returning
      value(RESULT) type ref to ZCL_UITB_CTM_NODE
    raising
      ZCX_UITB_TREE_ERROR .
  methods GET_NEXT_SIBLING
    returning
      value(RESULT) type ref to ZCL_UITB_CTM_NODE
    raising
      ZCX_UITB_TREE_ERROR .
  methods GET_NR_OF_CHILDREN
    returning
      value(RESULT) type I
    raising
      ZCX_UITB_TREE_ERROR .
  methods GET_PARENT
    returning
      value(RESULT) type ref to ZCL_UITB_CTM_NODE
    raising
      ZCX_UITB_TREE_ERROR .
  methods GET_PREVIOUS_SIBLING
    returning
      value(RESULT) type ref to ZCL_UITB_CTM_NODE
    raising
      ZCX_UITB_TREE_ERROR .
  methods GET_PROPERTIES
    returning
      value(RESULT) type TREEMSNOD
    raising
      ZCX_UITB_TREE_ERROR .
  methods GET_USER_OBJECT
    returning
      value(RESULT) type ref to OBJECT
    raising
      ZCX_UITB_TREE_ERROR .
  methods HAS_CHILDREN
    returning
      value(RESULT) type ABAP_BOOL
    raising
      ZCX_UITB_TREE_ERROR .
  methods HAS_EXPANDER
    returning
      value(RESULT) type ABAP_BOOL .
  methods IS_EXPANDED
    returning
      value(RESULT) type ABAP_BOOL .
  methods IS_DISABLED
    returning
      value(RESULT) type ABAP_BOOL .
  methods IS_FIRST_CHILD
    returning
      value(RF_IS_FIRST_CHILD) type ABAP_BOOL
    raising
      ZCX_UITB_TREE_ERROR .
  methods IS_FOLDER
    returning
      value(RESULT) type ABAP_BOOL .
  methods IS_LAST_CHILD
    returning
      value(RF_IS_LAST_CHILD) type ABAP_BOOL
    raising
      ZCX_UITB_TREE_ERROR .
  methods MOVE_NODE_TO
    importing
      !IV_RELATIVE_NODE type TM_NODEKEY
      !IV_RELATIONSHIP type I
    raising
      ZCX_UITB_TREE_ERROR .
  methods SET_DISABLED
    importing
      !VALUE type ABAP_BOOL default ABAP_TRUE
    raising
      ZCX_UITB_TREE_ERROR .
  methods SET_EXPANDED_IMAGE
    importing
      !VALUE type TV_IMAGE
    raising
      ZCX_UITB_TREE_ERROR .
  methods SET_EXPANDER
    importing
      !VALUE type ABAP_BOOL default ABAP_TRUE
    raising
      ZCX_UITB_TREE_ERROR .
  methods SET_FOLDER
    importing
      !VALUE type ABAP_BOOL default ABAP_TRUE
    raising
      ZCX_UITB_TREE_ERROR .
  methods SET_IMAGE
    importing
      !VALUE type TV_IMAGE
    raising
      ZCX_UITB_TREE_ERROR .
  methods SET_STYLE
    importing
      !IV_STYLE type I
    raising
      ZCX_UITB_TREE_ERROR .
  methods SET_USER_OBJECT
    importing
      !VALUE type ref to OBJECT
    raising
      ZCX_UITB_TREE_ERROR .
  methods TOGGLE .
  methods GET_NEXT_RELAT_NODE
    exporting
      !EF_IS_SIBLING type ABAP_BOOL
    returning
      value(RR_NODE) type ref to ZCL_UITB_CTM_NODE .
  PROTECTED SECTION.
private section.

  data MR_MODEL type ref to CL_COLUMN_TREE_MODEL .
  data MR_NODES type ref to ZCL_UITB_CTM_NODES .
  data MS_PROPERTIES type TREEMSNOD .
ENDCLASS.



CLASS ZCL_UITB_CTM_NODE IMPLEMENTATION.


  METHOD constructor.
    mv_node_key = iv_node_key.
    mr_model = ir_model.
    mr_nodes = ir_nodes.
    ms_properties = is_properties.

    IF ms_properties IS INITIAL.
      TRY.
          get_properties( ).
        CATCH zcx_uitb_tree_error ##needed.
*....... should not happen as the node object will not be created if there
*....... is no node for this key
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD delete_children.
    mr_model->node_get_children(
      EXPORTING
        node_key       = mv_node_key
      IMPORTING
        node_key_table = DATA(lt_children)
      EXCEPTIONS
        node_not_found = 1
        OTHERS         = 2
    ).
    IF sy-subrc <> 0.
      zcx_uitb_tree_error=>raise_from_sy( ).
    ELSE.
      CHECK lt_children IS NOT INITIAL.
      mr_model->delete_nodes( lt_children ).
    ENDIF.
  ENDMETHOD.


  METHOD get_children.
    mr_model->node_get_children(
      EXPORTING
        node_key       = mv_node_key
      IMPORTING
        node_key_table = DATA(lt_children)
      EXCEPTIONS
        node_not_found = 1
        OTHERS         = 2
    ).
    IF sy-subrc <> 0.
      zcx_uitb_tree_error=>raise_from_sy( ).
    ELSE.
      CHECK lt_children IS NOT INITIAL.

      LOOP AT lt_children ASSIGNING FIELD-SYMBOL(<lv_node>).
        rt_children = VALUE #( BASE rt_children
          ( NEW zcl_uitb_ctm_node(
              iv_node_key   = <lv_node>
              ir_model      = mr_model
              ir_nodes      = mr_nodes ) )
        ).
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD get_expanded_image.
    result = ms_properties-exp_image.
  ENDMETHOD.


  METHOD get_first_child.
    mr_model->node_get_first_child(
      EXPORTING
        node_key        = mv_node_key
      IMPORTING
        child_node_key  = DATA(lv_child)
      EXCEPTIONS
        node_not_found  = 1
        OTHERS          = 2
    ).
    IF sy-subrc <> 0.
      zcx_uitb_tree_error=>raise_from_sy( ).
    ELSE.
      CHECK lv_child IS NOT INITIAL.
      result = NEW #(
        iv_node_key = lv_child
        ir_model    = mr_model
        ir_nodes    = mr_nodes
      ).
    ENDIF.
  ENDMETHOD.


  METHOD get_image.
    result = ms_properties-n_image.
  ENDMETHOD.


  METHOD get_item.
    mr_model->node_get_item(
      EXPORTING
        node_key       = mv_node_key
        item_name      = iv_item_name
      IMPORTING
        item           = DATA(ls_item)
      EXCEPTIONS
        node_not_found = 1
        item_not_found = 2
        OTHERS         = 3
    ).
    IF sy-subrc <> 0.
      zcx_uitb_tree_error=>raise_from_sy( ).
    ELSE.
      rr_item = NEW zcl_uitb_ctm_item(
          ir_model           = mr_model
          iv_node_key        = mv_node_key
          iv_item_name       = iv_item_name
          is_item_properties = ls_item
      ).
    ENDIF.
  ENDMETHOD.


  METHOD get_items.
    mr_model->node_get_items(
      EXPORTING
        node_key       = mv_node_key
      IMPORTING
        item_table     = DATA(lt_items)
      EXCEPTIONS
        node_not_found = 1
        OTHERS         = 2
    ).
    IF sy-subrc <> 0.
      zcx_uitb_tree_error=>raise_from_sy( ).
    ELSE.
      LOOP AT lt_items ASSIGNING FIELD-SYMBOL(<ls_item>).
        result = VALUE #(
         BASE result
         ( NEW zcl_uitb_ctm_item(
              ir_model           = mr_model
              iv_node_key        = mv_node_key
              iv_item_name       = <ls_item>-item_name
              is_item_properties = <ls_item> )
         )
        ).
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD get_last_child.
    mr_model->node_get_last_child(
      EXPORTING
        node_key        = mv_node_key
      IMPORTING
        child_node_key  = DATA(lv_child)
      EXCEPTIONS
        node_not_found  = 1
        OTHERS          = 2
    ).
    IF sy-subrc <> 0.
      zcx_uitb_tree_error=>raise_from_sy( ).
    ELSE.
      CHECK lv_child IS NOT INITIAL.
      result = NEW #(
        iv_node_key = lv_child
        ir_model    = mr_model
        ir_nodes    = mr_nodes
      ).
    ENDIF.
  ENDMETHOD.


  METHOD get_next_relat_node.
    ef_is_sibling = abap_true.

    DATA(lr_next_node) = get_previous_sibling( ).
    IF lr_next_node IS INITIAL.
      lr_next_node = get_next_sibling( ).
      IF lr_next_node IS INITIAL.
        lr_next_node = get_parent( ).
        CLEAR ef_is_sibling.
      ENDIF.
    ENDIF.

    rr_node = lr_next_node.
  ENDMETHOD.


  METHOD get_next_sibling.
    mr_model->node_get_next_sibling(
      EXPORTING
        node_key        = mv_node_key
      IMPORTING
        sibling_node_key = DATA(lv_sibling)
      EXCEPTIONS
        node_not_found  = 1
        OTHERS          = 2
    ).
    IF sy-subrc <> 0.
      zcx_uitb_tree_error=>raise_from_sy( ).
    ELSE.
      CHECK lv_sibling IS NOT INITIAL.

      result = NEW #(
        iv_node_key = lv_sibling
        ir_model    = mr_model
        ir_nodes    = mr_nodes
      ).
    ENDIF.
  ENDMETHOD.


  METHOD get_nr_of_children.
    mr_model->node_get_nr_of_children(
      EXPORTING
        node_key       = mv_node_key
      IMPORTING
        nr_of_children = result
      EXCEPTIONS
        node_not_found = 1
        OTHERS         = 2
    ).
    IF sy-subrc <> 0.
      zcx_uitb_tree_error=>raise_from_sy( ).
    ENDIF.
  ENDMETHOD.


  METHOD get_parent.
    mr_model->node_get_parent(
      EXPORTING
        node_key        = mv_node_key
      IMPORTING
        parent_node_key = DATA(lv_parent)
      EXCEPTIONS
        node_not_found  = 1
        OTHERS          = 2
    ).
    IF sy-subrc <> 0.
      zcx_uitb_tree_error=>raise_from_sy( ).
    ELSE.
      CHECK lv_parent IS NOT INITIAL.
      result = NEW #(
        iv_node_key = lv_parent
        ir_model    = mr_model
        ir_nodes    = mr_nodes
      ).
    ENDIF.
  ENDMETHOD.


  METHOD get_previous_sibling.
    mr_model->node_get_prev_sibling(
      EXPORTING
        node_key        = mv_node_key
      IMPORTING
        sibling_node_key = DATA(lv_sibling)
      EXCEPTIONS
        node_not_found  = 1
        OTHERS          = 2
    ).
    IF sy-subrc <> 0.
      zcx_uitb_tree_error=>raise_from_sy( ).
    ELSE.
      CHECK lv_sibling IS NOT INITIAL.
      result = NEW #(
        iv_node_key = lv_sibling
        ir_model    = mr_model
        ir_nodes    = mr_nodes
      ).
    ENDIF.
  ENDMETHOD.


  METHOD get_properties.
    mr_model->node_get_properties(
      EXPORTING
        node_key       = mv_node_key
      IMPORTING
        properties     = result
      EXCEPTIONS
        node_not_found = 1
        OTHERS         = 2
    ).
    IF sy-subrc <> 0.
      zcx_uitb_tree_error=>raise_from_sy( ).
    ELSE.
      ms_properties = result.
    ENDIF.
  ENDMETHOD.


  METHOD get_user_object.
    mr_model->node_get_user_object(
      EXPORTING
        node_key       = mv_node_key
      IMPORTING
        user_object    = result
      EXCEPTIONS
        node_not_found = 1
        OTHERS         = 2
    ).
    IF sy-subrc <> 0.
      zcx_uitb_tree_error=>raise_from_sy( ).
    ENDIF.
  ENDMETHOD.


  METHOD has_children.
    result = xsdbool( get_nr_of_children( ) > 0 ).
  ENDMETHOD.


  METHOD has_expander.
    result = ms_properties-expander.
  ENDMETHOD.


  METHOD is_disabled.
    result = ms_properties-disabled.
  ENDMETHOD.


  METHOD is_expanded.
    CHECK is_folder( ).

    DATA(lt_expanded_nodes) = mr_nodes->get_expanded_nodes( ).
    IF line_exists( lt_expanded_nodes[ table_line = mv_node_key ] ).
      result = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD is_first_child.
    DATA(lr_parent) = get_parent( ).
*.. Does it have a parent at all?
    CHECK lr_parent IS BOUND.

*.. Retrieve the first child and check it check it against self
    DATA(lr_first_child) = lr_parent->get_first_child( ).

    rf_is_first_child = xsdbool( lr_first_child->mv_node_key = mv_node_key ).

  ENDMETHOD.


  METHOD is_folder.
    result = ms_properties-isfolder.
  ENDMETHOD.


  METHOD is_last_child.
    DATA(lr_parent) = get_parent( ).
*.. Does it have a parent at all?
    CHECK lr_parent IS BOUND.

*.. Retrieve the last child and check it check it against self
    DATA(lr_last_child) = lr_parent->get_last_child( ).

    rf_is_last_child = xsdbool( lr_last_child->mv_node_key = mv_node_key ).
  ENDMETHOD.


  METHOD move_node_to.
    mr_model->move_node(
      EXPORTING
        node_key                = mv_node_key
        relative_node_key       = iv_relative_node
        relationship            = iv_relationship
      EXCEPTIONS
        control_not_existing    = 1
        control_dead            = 2
        failed                  = 3
        cntl_system_error       = 4
        node_not_found          = 5
        move_error              = 6
        relative_node_not_found = 7
        illegal_relationship    = 8
        OTHERS                  = 9
    ).
    IF sy-subrc <> 0.
      zcx_uitb_tree_error=>raise_from_sy( ).
    ENDIF.
  ENDMETHOD.


  METHOD set_disabled.
    mr_model->node_set_disabled(
      EXPORTING
        node_key       = mv_node_key
        disabled       = value
      EXCEPTIONS
        node_not_found = 1
        OTHERS         = 2
    ).
    IF sy-subrc <> 0.
      zcx_uitb_tree_error=>raise_from_sy( ).
    ENDIF.

  ENDMETHOD.


  METHOD set_expanded_image.
    mr_model->node_set_expanded_image(
      EXPORTING
        node_key       = mv_node_key
        exp_image      = value
      EXCEPTIONS
        node_not_found = 1
        OTHERS         = 2
    ).
    IF sy-subrc <> 0.
      zcx_uitb_tree_error=>raise_from_sy( ).
    ENDIF.

  ENDMETHOD.


  METHOD set_expander.
    mr_model->node_set_expander(
      EXPORTING
        node_key       = mv_node_key
        expander       = value
      EXCEPTIONS
        node_not_found = 1
        OTHERS         = 2
    ).
    IF sy-subrc <> 0.
      zcx_uitb_tree_error=>raise_from_sy( ).
    ENDIF.

  ENDMETHOD.


  METHOD set_folder.
    mr_model->node_set_is_folder(
      EXPORTING
        node_key       = mv_node_key
        is_folder      = value
      EXCEPTIONS
        node_not_found = 1
        OTHERS         = 2
    ).
    IF sy-subrc <> 0.
      zcx_uitb_tree_error=>raise_from_sy( ).
    ENDIF.

  ENDMETHOD.


  METHOD set_image.
    mr_model->node_set_image(
      EXPORTING
        node_key       = mv_node_key
        image          = value
      EXCEPTIONS
        node_not_found = 1
        OTHERS         = 2
    ).
    IF sy-subrc <> 0.
      zcx_uitb_tree_error=>raise_from_sy( ).
    ENDIF.

  ENDMETHOD.


  METHOD set_style.
    mr_model->node_set_style(
      EXPORTING
        node_key       = mv_node_key
        style          = iv_style
      EXCEPTIONS
        node_not_found = 1
        OTHERS         = 2
    ).
    IF sy-subrc <> 0.
      zcx_uitb_tree_error=>raise_from_sy( ).
    ENDIF.
  ENDMETHOD.


  METHOD set_user_object.
    mr_model->node_set_user_object(
      EXPORTING
        node_key       = mv_node_key
        user_object    = value
      EXCEPTIONS
        node_not_found = 1
        OTHERS         = 2
    ).
    IF sy-subrc <> 0.
      zcx_uitb_tree_error=>raise_from_sy( ).
    ENDIF.

  ENDMETHOD.


  METHOD toggle.
    CHECK is_folder( ).

    DATA(lt_expanded) = mr_nodes->get_expanded_nodes( ).

    IF  line_exists( lt_expanded[ table_line = mv_node_key ] ).
      mr_nodes->collapse_node(
          iv_node_key = mv_node_key
      ).
    ELSE.
      mr_nodes->expand_node(
          iv_node_key = mv_node_key
      ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
