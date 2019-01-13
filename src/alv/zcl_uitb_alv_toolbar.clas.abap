CLASS zcl_uitb_alv_toolbar DEFINITION
  PUBLIC
  CREATE PUBLIC

  GLOBAL FRIENDS zcl_uitb_alv_functions .

  PUBLIC SECTION.

    INTERFACES zif_uitb_alv_function_holder .
    INTERFACES zif_uitb_alv_types.

    ALIASES tt_toolbar_button
      FOR zif_uitb_alv_types~tt_alv_toolbar_button.
    ALIASES ty_toolbar_button
      FOR zif_uitb_alv_types~ty_alv_toolbar_button.
    ALIASES ty_toolbar_menu
      FOR zif_uitb_alv_types~ty_alv_toolbar_menu.
    ALIASES tt_toolbar_menu
      FOR zif_uitb_alv_types~tt_alv_toolbar_menu.

    METHODS constructor .
    METHODS add_button
      IMPORTING
        !is_button          TYPE stb_button
        !iv_tag             TYPE string OPTIONAL
        !if_insert_at_front TYPE abap_bool OPTIONAL .
    METHODS add_seperator
      IMPORTING
        !if_insert_at_front TYPE abap_bool OPTIONAL .
    METHODS add_menu_button
      IMPORTING
        !is_button          TYPE stb_button
        !iv_tag             TYPE string OPTIONAL
        !if_insert_at_front TYPE abap_bool OPTIONAL
        !ir_menu            TYPE REF TO cl_ctmenu .
    METHODS remove_button
      IMPORTING
        !iv_function TYPE ui_func OPTIONAL
        iv_tag       TYPE string OPTIONAL.
    METHODS clear .
  PROTECTED SECTION.

    ALIASES function_registered
      FOR zif_uitb_alv_function_holder~function_registered .
    ALIASES unregister_function
      FOR zif_uitb_alv_function_holder~unregister_function .
    ALIASES unregister_functions
      FOR zif_uitb_alv_function_holder~unregister_functions .

    DATA mt_buttons TYPE tt_toolbar_button.
    DATA mt_buttons_front TYPE tt_toolbar_button.
    DATA mt_menus TYPE tt_toolbar_menu.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_UITB_ALV_TOOLBAR IMPLEMENTATION.


  METHOD add_button.
    DATA(ls_button) = CORRESPONDING ty_toolbar_button( is_button ).
    ls_button-tag = iv_tag.

    IF if_insert_at_front = abap_true.
      mt_buttons_front = VALUE #( BASE mt_buttons_front ( ls_button ) ).
    ELSE.
      mt_buttons = VALUE #( BASE mt_buttons ( ls_button ) ).
    ENDIF.

    RAISE EVENT function_registered
      EXPORTING
        es_function = VALUE zif_uitb_alv_types=>ty_alv_function(
            function        = is_button-function
            tag             = iv_tag
            checked         = is_button-checked
            disabled        = is_button-disabled
            in_toolbar      = abap_true
        ).
  ENDMETHOD.


  METHOD add_menu_button.
    DATA(ls_button) = CORRESPONDING ty_toolbar_button( is_button ).
    ls_button-tag = iv_tag.

    IF if_insert_at_front = abap_true.
      mt_buttons_front = VALUE #( BASE mt_buttons_front ( ls_button ) ).
    ELSE.
      mt_buttons = VALUE #( BASE mt_buttons ( ls_button ) ).
    ENDIF.

    mt_menus = VALUE #( BASE mt_menus
      ( function = is_button-function
        tag      = iv_tag
        ctmenu   = ir_menu
      )
    ).

    RAISE EVENT function_registered
      EXPORTING
        es_function = VALUE zif_uitb_alv_types=>ty_alv_function(
            function        = is_button-function
            tag             = iv_tag
            checked         = is_button-checked
            disabled        = is_button-disabled
            in_toolbar      = abap_true
        ).
  ENDMETHOD.


  METHOD add_seperator.
    IF if_insert_at_front = abap_true.
      mt_buttons_front = VALUE #( BASE mt_buttons_front ( butn_type = cntb_btype_sep ) ).
    ELSE.
      mt_buttons = VALUE #( BASE mt_buttons ( butn_type = cntb_btype_sep ) ).
    ENDIF.
  ENDMETHOD.


  METHOD clear.
    CLEAR: mt_buttons,
           mt_buttons_front,
           mt_menus.

    RAISE EVENT unregister_functions
      EXPORTING
        ef_toolbar      = abap_true.
  ENDMETHOD.


  METHOD constructor.
  ENDMETHOD.


  METHOD remove_button.
    DELETE mt_buttons WHERE function = iv_function
                         OR tag      = iv_tag.
    DATA(lf_trigger_unregister) = xsdbool( sy-subrc = 0 ).

    DELETE mt_menus WHERE function = iv_function
                       OR tag      = iv_tag.

    IF lf_trigger_unregister = abap_true.
      RAISE EVENT unregister_function
        EXPORTING
          es_function = VALUE zif_uitb_alv_types=>ty_alv_function(
              function        = iv_function
              tag             = iv_tag
              in_toolbar      = abap_true
          ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
