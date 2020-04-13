*&---------------------------------------------------------------------*
*& Report ZAB_SEL_SCREEN_DEMO
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zab_sel_screen_demo.

TABLES:
  sflight.

INCLUDE zab_selscreen_controller.

PARAMETERS: r1 RADIOBUTTON GROUP rad DEFAULT 'X' USER-COMMAND state,
            r2 RADIOBUTTON GROUP rad.

SELECT-OPTIONS: so_carr FOR sflight-carrid,
                so_conn FOR sflight-connid.

DATA:
  go_screen_carr TYPE REF TO lcl_sel_screen,
  go_screen_conn TYPE REF TO lcl_sel_screen.

INITIALIZATION.
  go_screen_carr = NEW #( ).
  go_screen_carr->set_text( iv_field = 'SO_CARR' iv_text  = 'Carrier ID' ).
  go_screen_carr->enable_options( iv_field   = 'SO_CARR' iv_options = 'EQ' ).
  go_screen_carr->set_active( iv_field = 'SO_CONN' iv_state = abap_false ).
  go_screen_carr->set_text( iv_field = 'R1' iv_text = 'State 1' ).
  go_screen_carr->set_text( iv_field = 'R2' iv_text = 'State 2' ).
  go_screen_carr->set_btn(
    EXPORTING
      iv_id   = 1
      iv_text = 'First btn'
      iv_icon = icon_led_green
      iv_info = 'First btn' ).
  go_screen_carr->exclude_command( 'ONLI' ).

  go_screen_conn = NEW #( ).
  go_screen_conn->set_text( iv_field = 'SO_CONN' iv_text  = 'Conn ID' ).
  go_screen_conn->enable_options( iv_field   = 'SO_CONN' iv_options = 'BT EQ' ).
  go_screen_conn->set_active( iv_field = 'SO_CARR' iv_state = abap_false ).
  go_screen_conn->set_text( iv_field = 'R1' iv_text = 'State 1' ).
  go_screen_conn->set_text( iv_field = 'R2' iv_text = 'State 2' ).
  go_screen_conn->set_btn(
    EXPORTING
      iv_id   = 1
      iv_text = 'Second btn'
      iv_icon = icon_led_red
      iv_info = 'Second btn' ).

AT SELECTION-SCREEN OUTPUT.
  CASE abap_true.
    WHEN r1.
      go_screen_carr->show( ).
    WHEN r2.
      go_screen_conn->show( ).
  ENDCASE.
