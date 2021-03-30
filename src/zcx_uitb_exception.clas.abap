"! <p class="shorttext synchronized" lang="en">General Error</p>
CLASS zcx_uitb_exception DEFINITION
  PUBLIC
  INHERITING FROM zcx_uitb_nc_exception
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS:
      BEGIN OF general_error,
        msgid TYPE symsgid VALUE 'ZUITB_EXCEPTION',
        msgno TYPE symsgno VALUE '000',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE 'MSGV3',
        attr4 TYPE scx_attrname VALUE 'MSGV4',
      END OF general_error,
      BEGIN OF cancelled_by_user,
        msgid TYPE symsgid VALUE 'ZUITB_EXCEPTION',
        msgno TYPE symsgno VALUE '002',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF cancelled_by_user.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.


CLASS zcx_uitb_exception IMPLEMENTATION.

ENDCLASS.
