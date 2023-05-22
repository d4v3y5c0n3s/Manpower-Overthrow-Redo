with allegro5_events_h; use allegro5_events_h;
with allegro5_system_h; use allegro5_system_h;
with allegro5_base_h; use allegro5_base_h;
with allegro5_keyboard_h; use allegro5_keyboard_h;
with allegro_primitives_h; use allegro_primitives_h;
with allegro_image_h; use allegro_image_h;
with allegro5_display_h; use allegro5_display_h;
with allegro5_color_h; use allegro5_color_h;
with Interfaces.C; use Interfaces.C;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Calendar; use Ada.Calendar;

procedure Manpoweroverthrowredoada is
  type Possible_States is (MainMenu, InLevel, ExitGame);
  type Game_State (S : Possible_States) is record
    case S is
      when MainMenu =>
        Test1 : Integer;
      when InLevel =>
        Test2 : Integer;
      when ExitGame =>
        null;
    end case;
  end record;
  type Game_State_Access is access Game_State;
  
  function init_game_state return Game_State_Access is
  begin
    return new Game_State'(S => MainMenu, Test1 => 7);
  end init_game_state;
  procedure End_Game_State (State : out Game_State_Access) is
  begin
    State := new Game_State'(S => ExitGame);
  end End_Game_State;
  procedure Draw_Game (State_In : in Game_State_Access) is
  begin
    case State_In.S is
      when MainMenu =>
        --:= al_map_rgb(0, 0, 0);
        --:= al_map_rgb(255, 255, 255);
        Put_Line("MainMenu!");
      when InLevel =>
        null;
      when ExitGame =>
        null;
    end case;
    al_flip_display;
  end Draw_Game;
  -- procedure Update_State
  UpdateInterval : constant Duration := 0.033;
  Frame_Start_Time : Time;
  FrameTime : Duration;
  GS : Game_State_Access;
  Q : access ALLEGRO_EVENT_QUEUE;
  Display : access ALLEGRO_DISPLAY;
  DisplayEventSrc : access ALLEGRO_EVENT_SOURCE;
  KBEventSrc : access ALLEGRO_EVENT_SOURCE;
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
      --exit when GS.S = ExitGame;
      FrameTime := Clock - Frame_Start_Time;
      if FrameTime < UpdateInterval then
        delay UpdateInterval - FrameTime;
      end if;
      End_Game_State(GS);
      exit when GS.S = ExitGame;
    end loop;
    Put_Line("Success.");
    al_destroy_event_queue(Q);
    al_destroy_display(Display);
  else
    Put_Line("Failure.");
  end if;
end Manpoweroverthrowredoada;
