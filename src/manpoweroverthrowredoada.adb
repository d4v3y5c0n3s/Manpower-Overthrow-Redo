with allegro5_events_h; use allegro5_events_h;
with allegro5_system_h; use allegro5_system_h;
with allegro5_base_h; use allegro5_base_h;
with allegro5_keyboard_h; use allegro5_keyboard_h;
with allegro_primitives_h; use allegro_primitives_h;
with allegro_image_h; use allegro_image_h;
with allegro5_display_h; use allegro5_display_h;
with allegro5_color_h; use allegro5_color_h;
with allegro5_drawing_h; use allegro5_drawing_h;
with allegro_font_h; use allegro_font_h;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Calendar; use Ada.Calendar;

procedure Manpoweroverthrowredoada is
  type Possible_States is (MainMenu, InLevel, ExitGame);
  
  type Game_State (S : Possible_States) is record
    case S is
      when MainMenu =>
        F : access ALLEGRO_FONT;
      when InLevel =>
        Test2 : Integer;
      when ExitGame =>
        null;
    end case;
  end record;
  
  type Game_State_Access is access Game_State;
  
  type Key_State is (Unpressed, Pressed);
  
  type Input_State is record
    Esc_KS : Key_State := Unpressed;
  end record;
  
  function init_game_state return Game_State_Access is
    DefaultFont : constant access ALLEGRO_FONT := al_create_builtin_font;
  begin
    return new Game_State'(S => MainMenu, F => DefaultFont);
  end init_game_state;
  
  procedure Draw_Game (State_In : in Game_State_Access) is
  begin
    case State_In.S is
      when MainMenu =>
        al_clear_to_color(al_map_rgb(0, 0, 0));
        al_draw_text(State_In.F, al_map_rgb(255, 255, 255), 0.0, 0.0, 0, New_String("Press start or noob"));
      when InLevel =>
        null;
      when ExitGame =>
        null;
    end case;
    al_flip_display;
  end Draw_Game;
  
  procedure Update_Input (InpS : in out Input_State; Event : access ALLEGRO_EVENT) is
    package C_Type_IO is new Modular_IO(unsigned);
  begin
    Put("(");
    C_Type_IO.Put(Event.c_type);
    Put_Line(")");
    case Event.c_type is
      when 10 => -- key down
        if Event.keyboard.keycode = 59 then
          InpS.Esc_KS := Pressed;
        end if;
      when 12 => -- key up
        if Event.keyboard.keycode = 59 then
          InpS.Esc_KS := Unpressed;
        end if;
      when others =>
        null;
    end case;
  end Update_Input;
  
  procedure Update_State (
    State_In_Out : in out Game_State_Access;
    InpS : in out Input_State;
    Event : access ALLEGRO_EVENT) is
  begin
    case Event.c_type is
      when 42 => -- display close
        State_In_Out := new Game_State'(S => ExitGame);
      when others =>
        Update_Input(InpS, Event);
    end case;
    case State_In_Out.S is
      when MainMenu =>
        if InpS.Esc_KS = Pressed then
          State_In_Out := new Game_State'(S => ExitGame);
        end if;
      when InLevel =>
        null;
      when ExitGame =>
        null;
    end case;
  end Update_State;
  
  UpdateInterval : constant Duration := 0.033;
  Frame_Start_Time : Time;
  FrameTime : Duration;
  GS : Game_State_Access;
  Q : access ALLEGRO_EVENT_QUEUE;
  Display : access ALLEGRO_DISPLAY;
  DisplayEventSrc : access ALLEGRO_EVENT_SOURCE;
  KBEventSrc : access ALLEGRO_EVENT_SOURCE;
  Ev : access ALLEGRO_EVENT := new ALLEGRO_EVENT;
  InpState : Input_State;
begin
  if al_install_system(Interfaces.C.int(al_get_allegro_version), null) and
  al_install_keyboard and
  al_init_primitives_addon and
  al_init_image_addon then
    Q := al_create_event_queue;
    Display := al_create_display(600, 400);
    DisplayEventSrc := al_get_display_event_source(Display);
    al_register_event_source(Q, DisplayEventSrc);
    KBEventSrc := al_get_keyboard_event_source;
    al_register_event_source(Q, KBEventSrc);
    GS := init_game_state;
    loop
      Frame_Start_Time := Clock;
      Draw_Game(GS);
      if al_get_next_event(Q, Ev) then
        Update_State(GS, InpState, Ev);
      end if;
      exit when GS.S = ExitGame;
      FrameTime := Clock - Frame_Start_Time;
      if FrameTime < UpdateInterval then
        delay UpdateInterval - FrameTime;
      end if;
      --End_Game_State(GS);
    end loop;
    al_destroy_event_queue(Q);
    al_destroy_display(Display);
  else
    Put_Line("Failure.");
  end if;
end Manpoweroverthrowredoada;
