INTERFACE zif_uitb_alv_types
  PUBLIC .
  TYPES: BEGIN OF ty_alv_toolbar_menu.
          INCLUDE TYPE stb_btnmnu.
  TYPES tag TYPE string.
  TYPES: END OF ty_alv_toolbar_menu.

  TYPES: tt_alv_toolbar_menu TYPE STANDARD TABLE OF ty_alv_toolbar_menu WITH EMPTY KEY.

  TYPES: BEGIN OF ty_alv_toolbar_button.
          INCLUDE TYPE stb_button.
  TYPES tag TYPE string.
  TYPES: END OF ty_alv_toolbar_button.

  TYPES: tt_alv_toolbar_button TYPE STANDARD TABLE OF ty_alv_toolbar_button WITH EMPTY KEY.

  TYPES:
    BEGIN OF ty_alv_function_tag_map,
      function TYPE ui_func,
      tag      TYPE string,
    END OF ty_alv_function_tag_map.

  TYPES: tt_alv_function_tag_map TYPE STANDARD TABLE OF ty_alv_function_tag_map WITH KEY function.

  TYPES:
    BEGIN OF ty_alv_function,
      function        TYPE ui_func,
      tag             TYPE string,
      checked         TYPE abap_bool,
      disabled        TYPE abap_bool,
      in_toolbar      TYPE abap_bool,
      in_context_menu TYPE abap_bool,
    END OF ty_alv_function.

  TYPES: tt_alv_function TYPE STANDARD TABLE OF ty_alv_function WITH EMPTY KEY.

  TYPES:
    BEGIN OF ty_function,
      order                TYPE tabfdpos,
      type                 TYPE tb_btype,
      function             TYPE ui_func,
      icon                 TYPE iconname,
      quickinfo            TYPE iconquick,
      checked              TYPE abap_bool,
      text                 TYPE text40,
      tag                  TYPE string,
      show_text_in_toolbar TYPE abap_bool,
      for_toolbar          TYPE abap_bool,
      to_start_of_toolbar  TYPE abap_bool,
      for_context_menu     TYPE abap_bool,
      to_start_of_menu     TYPE abap_bool,
      disabled             TYPE abap_bool,
    END OF ty_function.

  TYPES: tt_function TYPE STANDARD TABLE OF ty_function WITH EMPTY KEY.

  TYPES:
    BEGIN OF ty_menu_function,
      function TYPE ui_func,
      text     TYPE text40,
      tag      TYPE string,
    END OF ty_menu_function.

  TYPES: tt_menu_function TYPE STANDARD TABLE OF ty_menu_function WITH EMPTY KEY.

  TYPES:
    BEGIN OF ty_toolbar_menu,
      icon      TYPE iconname,
      function  TYPE ui_func,
      quickinfo TYPE iconquick,
      functions TYPE tt_menu_function,
      menu_ref  TYPE REF TO cl_ctmenu,
    END OF ty_toolbar_menu.

  TYPES: tt_toolbar_menu TYPE STANDARD TABLE OF ty_toolbar_menu WITH EMPTY KEY.

  TYPES:
    BEGIN OF ty_alv_layout_key,
      report        TYPE repid,
      handle        TYPE slis_handl,
      logical_group TYPE slis_loggr,
    END OF ty_alv_layout_key.

  TYPES:
    BEGIN OF ty_alv_layout,
      layout        TYPE slis_vari,
      text          TYPE string,
      user_specific TYPE abap_bool,
      default       TYPE abap_bool,
    END OF ty_alv_layout.

  TYPES:
    BEGIN OF ty_s_dropdown_value,
      value     TYPE char128,
      int_value TYPE char128,
    END OF ty_s_dropdown_value.

  TYPES: tt_dropdown_value TYPE STANDARD TABLE OF ty_s_dropdown_value WITH DEFAULT KEY.

  TYPES:
    BEGIN OF ty_s_dropdown_ref,
      handle       TYPE i,
      dropdown_ref TYPE REF TO zcl_uitb_alv_dropdown,
    END OF ty_s_dropdown_ref.

  TYPES: tt_dropdown_ref TYPE STANDARD TABLE OF ty_s_dropdown_ref WITH KEY handle.


  TYPES:
    BEGIN OF ty_cell,
      row    TYPE i,
      column TYPE lvc_fname,
    END OF ty_cell.

  TYPES: tt_cell TYPE STANDARD TABLE OF ty_cell WITH DEFAULT KEY.
  TYPES:
    BEGIN OF ty_alv_s_changelist,
      name         TYPE string,
      flavour      TYPE i,
      object       TYPE string,
      method       TYPE string,
      sequence     TYPE i,
      change       TYPE abap_bool,
      refresh_mode TYPE salv_de_constant,
      ref          TYPE REF TO zcl_uitb_alv_metadata,
      frontend     TYPE abap_bool,
    END OF ty_alv_s_changelist .

  TYPES:
    tt_alv_changelist TYPE STANDARD TABLE OF ty_alv_s_changelist
      WITH KEY name flavour object method .

  TYPES:
    BEGIN OF ty_alv_filter,
      columnname TYPE lvc_fname,
      filter_ref TYPE REF TO zcl_uitb_alv_filter,
    END OF ty_alv_filter.

  TYPES: tt_alv_filter TYPE STANDARD TABLE OF ty_alv_filter WITH DEFAULT KEY.
ENDINTERFACE.
