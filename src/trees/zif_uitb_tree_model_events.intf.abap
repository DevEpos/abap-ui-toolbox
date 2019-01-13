interface ZIF_UITB_TREE_MODEL_EVENTS
  public .


  events DRAG
    exporting
      value(EV_NODE_KEY) type TM_NODEKEY
      value(EV_ITEM_NAME) type TV_ITMNAME
      value(ER_DRAG_DROP_OBJECT) type ref to CL_DRAGDROPOBJECT .
  events DRAG_MULTIPLE
    exporting
      value(ET_NODE_KEY_TABLE) type TREEMNOTAB
      value(EV_ITEM_NAME) type TV_ITMNAME
      value(ER_DRAG_DROP_OBJECT) type ref to CL_DRAGDROPOBJECT .
  events DROP_COMPLETE
    exporting
      value(EV_NODE_KEY) type TM_NODEKEY
      value(EV_ITEM_NAME) type TV_ITMNAME
      value(ER_DRAG_DROP_OBJECT) type ref to CL_DRAGDROPOBJECT .
  events DROP
    exporting
      value(EV_NODE_KEY) type TM_NODEKEY
      value(ER_DRAG_DROP_OBJECT) type ref to CL_DRAGDROPOBJECT .
  events DROP_COMPLETE_MULTIPLE
    exporting
      value(ET_NODE_KEY_TABLE) type TREEMNOTAB
      value(EV_ITEM_NAME) type TV_ITMNAME
      value(ER_DRAG_DROP_OBJECT) type ref to CL_DRAGDROPOBJECT .
  events NODE_CONTEXT_MENU_REQUEST
    exporting
      value(EV_NODE_KEY) type TM_NODEKEY
      value(ER_MENU) type ref to CL_CTMENU .
  events NODE_CONTEXT_MENU_SELECT
    exporting
      value(EV_NODE_KEY) type TM_NODEKEY
      value(EV_FCODE) type SY-UCOMM .
  events NODE_DOUBLE_CLICK
    exporting
      value(EV_NODE_KEY) type TM_NODEKEY .
  events NODE_KEYPRESS
    exporting
      value(EV_NODE_KEY) type TM_NODEKEY
      value(EV_KEY) type I .
  events SELECTION_CHANGED
    exporting
      value(EV_NODE_KEY) type TM_NODEKEY .
  events EXPAND_NO_CHILDREN
    exporting
      value(EV_NODE_KEY) type TM_NODEKEY .
  events BUTTON_CLICK
    exporting
      value(EV_NODE_KEY) type TM_NODEKEY
      value(EV_ITEM_NAME) type TV_ITMNAME .
  events LINK_CLICK
    exporting
      value(EV_NODE_KEY) type TM_NODEKEY
      value(EV_ITEM_NAME) type TV_ITMNAME .
  events ITEM_DOUBLE_CLICK
    exporting
      value(EV_NODE_KEY) type TM_NODEKEY
      value(EV_ITEM_NAME) type TV_ITMNAME .
  events ITEM_KEYPRESS
    exporting
      value(EV_NODE_KEY) type TM_NODEKEY
      value(EV_KEY) type I
      value(EV_ITEM_NAME) type TV_ITMNAME .
  events CHECKBOX_CHANGE
    exporting
      value(EV_NODE_KEY) type TM_NODEKEY
      value(EV_ITEM_NAME) type TV_ITMNAME
      value(EF_CHECKED) type ABAP_BOOL .
endinterface.
