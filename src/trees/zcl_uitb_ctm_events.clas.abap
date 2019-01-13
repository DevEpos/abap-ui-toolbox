class ZCL_UITB_CTM_EVENTS definition
  public
  final
  create public .

public section.

  interfaces ZIF_UITB_TREE_MODEL_EVENTS .

  aliases BUTTON_CLICK
    for ZIF_UITB_TREE_MODEL_EVENTS~BUTTON_CLICK .
  aliases CHECKBOX_CHANGE
    for ZIF_UITB_TREE_MODEL_EVENTS~CHECKBOX_CHANGE .
  aliases DRAG
    for ZIF_UITB_TREE_MODEL_EVENTS~DRAG .
  aliases DRAG_MULTIPLE
    for ZIF_UITB_TREE_MODEL_EVENTS~DRAG_MULTIPLE .
  aliases DROP
    for ZIF_UITB_TREE_MODEL_EVENTS~DROP .
  aliases DROP_COMPLETE
    for ZIF_UITB_TREE_MODEL_EVENTS~DROP_COMPLETE .
  aliases DROP_COMPLETE_MULTIPLE
    for ZIF_UITB_TREE_MODEL_EVENTS~DROP_COMPLETE_MULTIPLE .
  aliases EXPAND_NO_CHILDREN
    for ZIF_UITB_TREE_MODEL_EVENTS~EXPAND_NO_CHILDREN .
  aliases ITEM_DOUBLE_CLICK
    for ZIF_UITB_TREE_MODEL_EVENTS~ITEM_DOUBLE_CLICK .
  aliases ITEM_KEYPRESS
    for ZIF_UITB_TREE_MODEL_EVENTS~ITEM_KEYPRESS .
  aliases LINK_CLICK
    for ZIF_UITB_TREE_MODEL_EVENTS~LINK_CLICK .
  aliases NODE_CONTEXT_MENU_REQUEST
    for ZIF_UITB_TREE_MODEL_EVENTS~NODE_CONTEXT_MENU_REQUEST .
  aliases NODE_CONTEXT_MENU_SELECT
    for ZIF_UITB_TREE_MODEL_EVENTS~NODE_CONTEXT_MENU_SELECT .
  aliases NODE_DOUBLE_CLICK
    for ZIF_UITB_TREE_MODEL_EVENTS~NODE_DOUBLE_CLICK .
  aliases NODE_KEYPRESS
    for ZIF_UITB_TREE_MODEL_EVENTS~NODE_KEYPRESS .
  aliases SELECTION_CHANGED
    for ZIF_UITB_TREE_MODEL_EVENTS~SELECTION_CHANGED .

  methods CONSTRUCTOR
    importing
      !IR_MODEL type ref to CL_COLUMN_TREE_MODEL
      !IF_ITEM_SELECTION type ABAP_BOOL
      !IF_SINGLE_SELECTION type ABAP_BOOL .
  methods ADD_KEY_FOR_KEYPRESS
    importing
      !IV_KEY type I
    raising
      ZCX_UITB_TREE_ERROR .
  methods REMOVE_ALL_KEY_STROKES .
  PROTECTED SECTION.
private section.

  data MR_MODEL type ref to CL_COLUMN_TREE_MODEL .

  methods BUILD_EVENTS
    raising
      ZCX_UITB_TREE_ERROR .
  methods ON_NODE_DOUBLE_CLICK
    for event NODE_DOUBLE_CLICK of CL_COLUMN_TREE_MODEL
    importing
      !NODE_KEY .
  methods ON_TREE_DRAG
    for event DRAG of CL_COLUMN_TREE_MODEL
    importing
      !DRAG_DROP_OBJECT
      !ITEM_NAME
      !NODE_KEY .
  methods ON_TREE_DRAG_MULTIPLE
    for event DRAG_MULTIPLE of CL_COLUMN_TREE_MODEL
    importing
      !DRAG_DROP_OBJECT
      !ITEM_NAME
      !NODE_KEY_TABLE .
  methods ON_DROP_COMPLETE
    for event DROP_COMPLETE of CL_COLUMN_TREE_MODEL
    importing
      !DRAG_DROP_OBJECT
      !ITEM_NAME
      !NODE_KEY .
  methods ON_TREE_DROP_COMPLETE_MULTIPLE
    for event DROP_COMPLETE_MULTIPLE of CL_COLUMN_TREE_MODEL
    importing
      !DRAG_DROP_OBJECT
      !ITEM_NAME
      !NODE_KEY_TABLE .
  methods ON_TREE_DROP
    for event DROP of CL_COLUMN_TREE_MODEL
    importing
      !DRAG_DROP_OBJECT
      !NODE_KEY
      !SENDER .
  methods ON_NODE_CONTEXT_MENU_REQUEST
    for event NODE_CONTEXT_MENU_REQUEST of CL_COLUMN_TREE_MODEL
    importing
      !MENU
      !NODE_KEY .
  methods ON_NODE_CONTEXT_MENU_SELECT
    for event NODE_CONTEXT_MENU_SELECT of CL_COLUMN_TREE_MODEL
    importing
      !FCODE
      !NODE_KEY .
  methods ON_NODE_KEY_PRESS
    for event NODE_KEYPRESS of CL_COLUMN_TREE_MODEL
    importing
      !KEY
      !NODE_KEY .
  methods ON_ITEM_KEY_PRESS
    for event ITEM_KEYPRESS of CL_COLUMN_TREE_MODEL
    importing
      !ITEM_NAME
      !KEY
      !NODE_KEY .
  methods ON_ITEM_DOUBLE_CLICK
    for event ITEM_DOUBLE_CLICK of CL_COLUMN_TREE_MODEL
    importing
      !ITEM_NAME
      !NODE_KEY .
  methods ON_EXPAND_NO_CHILDREN
    for event EXPAND_NO_CHILDREN of CL_COLUMN_TREE_MODEL
    importing
      !NODE_KEY .
  methods ON_SELECTION_CHANGED
    for event SELECTION_CHANGED of CL_COLUMN_TREE_MODEL
    importing
      !NODE_KEY .
  methods ON_ITEM_BUTTON_CLICK
    for event BUTTON_CLICK of CL_COLUMN_TREE_MODEL
    importing
      !ITEM_NAME
      !NODE_KEY .
  methods ON_ITEM_LINK_CLICK
    for event LINK_CLICK of CL_COLUMN_TREE_MODEL
    importing
      !ITEM_NAME
      !NODE_KEY .
  methods ON_CHECKBOX_CHANGE
    for event CHECKBOX_CHANGE of CL_ITEM_TREE_MODEL
    importing
      !CHECKED
      !ITEM_NAME
      !NODE_KEY .
ENDCLASS.



CLASS ZCL_UITB_CTM_EVENTS IMPLEMENTATION.


  METHOD add_key_for_keypress.
    mr_model->add_key_stroke(
      EXPORTING
        key         = iv_key
      EXCEPTIONS
        illegal_key = 1
        OTHERS      = 2
    ).
    IF sy-subrc <> 0.
      zcx_uitb_tree_error=>raise_from_sy( ).
    ENDIF.
  ENDMETHOD.


  method BUILD_EVENTS.
  endmethod.


  METHOD constructor.
    DATA: lt_events TYPE cntl_simple_events.

*.. Selection change event only works if the following criteria are met
*.. - no item selection
*.. - no double click event registered
*.. - no multiple selection activated
    mr_model = ir_model.

    lt_events = VALUE #(
        ( eventid = cl_column_tree_model=>eventid_node_context_menu_req  appl_event = abap_true )
        ( eventid = cl_column_tree_model=>eventid_node_double_click      appl_event = abap_true )
        ( eventid = cl_column_tree_model=>eventid_node_keypress          appl_event = abap_true )
     ).

*... add default key stroke ENTER
    add_key_for_keypress( cl_tree_model=>key_enter ).

    IF if_item_selection = abap_true.
      lt_events = VALUE #(
          BASE lt_events
          ( eventid = cl_column_tree_model=>eventid_link_click        appl_event = abap_true )
          ( eventid = cl_column_tree_model=>eventid_button_click      appl_event = abap_true )
          ( eventid = cl_column_tree_model=>eventid_item_keypress     appl_event = abap_true )
          ( eventid = cl_column_tree_model=>eventid_item_double_click appl_event = abap_true )
          ( eventid = cl_column_tree_model=>eventid_checkbox_change   appl_event = abap_true )
      ).
    ENDIF.

    mr_model->set_registered_events( lt_events ).

    SET HANDLER:
      on_node_context_menu_request FOR mr_model,
      on_node_context_menu_select FOR mr_model,
      on_node_double_click FOR mr_model,
      on_tree_drag FOR mr_model,
      on_tree_drop FOR mr_model,
      on_node_key_press FOR mr_model,
      on_expand_no_children FOR mr_model.

    IF if_single_selection = abap_false.
      SET HANDLER:
        on_tree_drag_multiple FOR mr_model,
        on_tree_drop_complete_multiple FOR mr_model.
    ENDIF.

    IF if_item_selection = abap_true.
      SET HANDLER:
          on_item_button_click FOR mr_model,
          on_item_link_click FOR mr_model,
          on_item_key_press FOR mr_model,
          on_checkbox_change FOR mr_model,
          on_item_double_click FOR mr_model.
    ENDIF.
  ENDMETHOD.


  METHOD on_checkbox_change.
    RAISE EVENT checkbox_change
      EXPORTING
        ev_item_name = item_name
        ef_checked   = checked
        ev_node_key  = node_key.
  ENDMETHOD.


  METHOD on_drop_complete.
    RAISE EVENT drop_complete
      EXPORTING
        ev_node_key         = node_key
        ev_item_name        = item_name
        er_drag_drop_object = drag_drop_object.
  ENDMETHOD.


  METHOD on_expand_no_children.
    RAISE EVENT expand_no_children
      EXPORTING
        ev_node_key = node_key.
  ENDMETHOD.


  METHOD on_item_button_click.
    RAISE EVENT button_click
      EXPORTING
        ev_node_key  = node_key
        ev_item_name = item_name.
  ENDMETHOD.


  METHOD on_item_double_click.
    RAISE EVENT item_double_click
      EXPORTING
        ev_node_key  = node_key
        ev_item_name = item_name.
  ENDMETHOD.


  METHOD on_item_key_press.
    RAISE EVENT item_keypress
      EXPORTING
        ev_node_key  = node_key
        ev_item_name = item_name
        ev_key       = key.
  ENDMETHOD.


  METHOD on_item_link_click.
    RAISE EVENT link_click
      EXPORTING
        ev_node_key  = node_key
        ev_item_name = item_name.
  ENDMETHOD.


  METHOD on_node_context_menu_request.
    RAISE EVENT node_context_menu_request
      EXPORTING
        ev_node_key = node_key
        er_menu     = menu.
  ENDMETHOD.


  METHOD on_node_context_menu_select.
    RAISE EVENT node_context_menu_select
      EXPORTING
        ev_node_key = node_key
        ev_fcode    = fcode.
  ENDMETHOD.


  METHOD on_node_double_click.
    RAISE EVENT node_double_click
      EXPORTING
        ev_node_key = node_key.
  ENDMETHOD.


  METHOD on_node_key_press.
    RAISE EVENT node_keypress
      EXPORTING
        ev_node_key = node_key
        ev_key      = key.
  ENDMETHOD.


  METHOD on_selection_changed.
    RAISE EVENT selection_changed
      EXPORTING
        ev_node_key = node_key.
  ENDMETHOD.


  METHOD on_tree_drag.
    RAISE EVENT drag
      EXPORTING
        ev_node_key         = node_key
        ev_item_name        = item_name
        er_drag_drop_object = drag_drop_object.
  ENDMETHOD.


  METHOD on_tree_drag_multiple.
    RAISE EVENT drag_multiple
      EXPORTING
        et_node_key_table   = node_key_table
        ev_item_name        = item_name
        er_drag_drop_object = drag_drop_object.
  ENDMETHOD.


  METHOD on_tree_drop.
    RAISE EVENT drop
      EXPORTING
        ev_node_key         = node_key
        er_drag_drop_object = drag_drop_object.
  ENDMETHOD.


  METHOD on_tree_drop_complete_multiple.
    RAISE EVENT drop_complete_multiple
      EXPORTING
        et_node_key_table   = node_key_table
        ev_item_name        = item_name
        er_drag_drop_object = drag_drop_object.
  ENDMETHOD.


  METHOD remove_all_key_strokes.
    mr_model->remove_all_key_strokes( ).
  ENDMETHOD.
ENDCLASS.
