INTERFACE zif_uitb_c_alv_cell_style
  PUBLIC .

  CONSTANTS:
    style2_no_border_left(4)   TYPE x VALUE '00010000',
    style2_no_border_right(4)  TYPE x VALUE '00020000',
    style2_no_border_top(4)    TYPE x VALUE '00040000',
    style2_no_border_bottom(4) TYPE x VALUE '00080000',

    style4_zebra_row(4)        TYPE x VALUE '00000001',
    style4_stop_merge(4)       TYPE x VALUE '00000002',

    style4_link(4)             TYPE x VALUE '00000004',
    style4_link_no(4)          TYPE x VALUE '00000008',

    color_background           TYPE raw4 VALUE '00000001',
    color_heading              TYPE raw4 VALUE '00000002',
    color_normal               TYPE raw4 VALUE '00000003',
    color_total                TYPE raw4 VALUE '00000004',
    color_key                  TYPE raw4 VALUE '00000005',
    color_positive             TYPE raw4 VALUE '00000006',
    color_negative             TYPE raw4 VALUE '00000007',
    color_group                TYPE raw4 VALUE '00000008',
    color_int_background       TYPE raw4 VALUE '00000009',
    color_int_heading          TYPE raw4 VALUE '0000000A',
    color_int_normal           TYPE raw4 VALUE '0000000B',
    color_int_total            TYPE raw4 VALUE '0000000C',
    color_int_key              TYPE raw4 VALUE '0000000D',
    color_int_positive         TYPE raw4 VALUE '0000000E',
    color_int_negative         TYPE raw4 VALUE '0000000F',
    color_int_group            TYPE raw4 VALUE '00000010',
    color_inv_background       TYPE raw4 VALUE '00000011',
    color_inv_heading          TYPE raw4 VALUE '00000012',
    color_inv_normal           TYPE raw4 VALUE '00000013',
    color_inv_total            TYPE raw4 VALUE '00000014',
    color_inv_key              TYPE raw4 VALUE '00000015',
    color_inv_positive         TYPE raw4 VALUE '00000016',
    color_inv_negative         TYPE raw4 VALUE '00000017',
    color_inv_group            TYPE raw4 VALUE '00000018',

    font_bold                  TYPE raw4 VALUE '00000020',
    font_bold_no               TYPE raw4 VALUE '00000040',

    font_italic                TYPE raw4 VALUE '00000080',
    font_italic_no             TYPE raw4 VALUE '00000100',

    font_underlined            TYPE raw4 VALUE '00000200',
    font_underlined_no         TYPE raw4 VALUE '00000400',

    align_left_top             TYPE raw4 VALUE '00000800',
    align_center_top           TYPE raw4 VALUE '00001000',
    align_right_top            TYPE raw4 VALUE '00001800',
    align_left_center          TYPE raw4 VALUE '00002000',
    align_center_center        TYPE raw4 VALUE '00002800',
    align_right_center         TYPE raw4 VALUE '00003000',
    align_left_bottom          TYPE raw4 VALUE '00003800',
    align_center_bottom        TYPE raw4 VALUE '00004000',
    align_right_bottom         TYPE raw4 VALUE '00004800',

    font_symbol                TYPE raw4 VALUE '00008000',
    font_symbol_no             TYPE raw4 VALUE '00010000',

    checkbox_not_checked       TYPE raw4 VALUE '00020000',
    checkbox_checked           TYPE raw4 VALUE '00040000',
    checkbox_no                TYPE raw4 VALUE '00060000',

    enabled                    TYPE raw4 VALUE '00080000',
    disabled                   TYPE raw4 VALUE '00100000',

    single_clk_event           TYPE raw4 VALUE '00200000',
    single_clk_event_no        TYPE raw4 VALUE '00400000',

    radio_not_checked          TYPE raw4 VALUE '00800000',
    radio_checked              TYPE raw4 VALUE '01000000',
    radio_no                   TYPE raw4 VALUE '01800000',

    f4                         TYPE raw4 VALUE '02000000',
    f4_no                      TYPE raw4 VALUE '04000000',

    image                      TYPE raw4 VALUE '08000000',

    no_delete_row              TYPE raw4 VALUE '10000000',

    button                     TYPE raw4 VALUE '20000000',
    button_no                  TYPE raw4 VALUE '40000000'.
ENDINTERFACE.
