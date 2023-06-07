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
with Ada.Containers.Vectors;

procedure Manpoweroverthrowredoada is
  
  type Scalar is digits 9 range -1024.0 .. 1024.0;
  
  type Pos is record
    X : Scalar;
    Y : Scalar;
  end record;
  
  type Scene_Data_Options is (Player, Enemy, Tile);
  
  type Scene_Data (Ops : Scene_Data_Options) is record
    case Ops is
      when Player =>
        ID : Integer;
        PositionInWorld : Pos;
      when Enemy =>
        null;
      when Tile =>
        null;
    end case;
  end record;
  
  type Scene_Data_Access is access Scene_Data;
  
  package Scene is new Ada.Containers.Vectors(
    Index_Type => Natural,
    Element_Type => Scene_Data_Access
  );
  
  type Possible_States is (MainMenu, InLevel, ExitGame);
  
  type Game_State (S : Possible_States) is record
    F : access ALLEGRO_FONT := al_create_builtin_font;
    case S is
      when MainMenu =>
        null;
      when InLevel =>
        ScreenPos : Pos := Pos'(X => 0.0, Y => 0.0);
        Scn : Scene.Vector;
      when ExitGame =>
        null;
    end case;
  end record;
  
  type Game_State_Access is access Game_State;
  
  type Key_State is (Unpressed, Pressed);
  
  type Input_State is record
    Esc_KS : Key_State := Unpressed;
    Start_KS : Key_State := Unpressed;
    Mv_Up_KS : Key_State := Unpressed;
    Mv_Down_KS : Key_State := Unpressed;
    Mv_Left_KS : Key_State := Unpressed;
    Mv_Right_KS : Key_State := Unpressed;
  end record;
  
  PlayerMoveSpeed : constant Scalar := 1.0;
  
  function transform_by (P1 : Pos; P2 : Pos) return Pos is
  begin
    return Pos'(X => P1.X + P2.X, Y => P1.Y + P2.Y);
  end transform_by;
  
  procedure Draw_Game (State_In : in Game_State_Access) is
  begin
    case State_In.S is
      when MainMenu =>
        al_clear_to_color(al_map_rgb(0, 0, 0));
        al_draw_text(State_In.F, al_map_rgb(255, 255, 255), 0.0, 0.0, 0, New_String("Press start or noob"));
      when InLevel =>
        al_clear_to_color(al_map_rgb(200, 200, 200));
        for E of State_In.Scn loop
          case E.Ops is
            when Player =>
              PlayerStuff:
                declare
                  TL : Pos := E.PositionInWorld;
                  BR : Pos;
                begin
                  TL := transform_by(TL, State_In.ScreenPos);
                  BR := transform_by(TL, Pos'(X => 100.0, Y => 100.0));
                  al_draw_filled_rectangle(Float(TL.X), Float(TL.Y), Float(BR.X), Float(BR.Y), al_map_rgb(125, 125, 125));
                end PlayerStuff;
              null;
            when Enemy =>
              null;
            when Tile =>
              null;
          end case;
        end loop;
        al_draw_text(State_In.F, al_map_rgb(20, 20, 20), 0.0, 0.0, 0, New_String("Yo, it's a level"));
      when ExitGame =>
        null;
    end case;
    al_flip_display;
  end Draw_Game;
  
  procedure Update_Input (InpS : in out Input_State; Event : access ALLEGRO_EVENT) is
  begin
    case Event.c_type is
      when 10 => -- key down
        if Event.keyboard.keycode = 59 then
          InpS.Esc_KS := Pressed;
        end if;
        if Event.keyboard.keycode = 67 then
          InpS.Start_KS := Pressed;
        end if;
        if Event.keyboard.keycode = 23 then
          InpS.Mv_Up_KS := Pressed;
        end if;
        if Event.keyboard.keycode = 19 then
          InpS.Mv_Down_KS := Pressed;
        end if;
        if Event.keyboard.keycode = 1 then
          InpS.Mv_Left_KS := Pressed;
        end if;
        if Event.keyboard.keycode = 4 then
          InpS.Mv_Right_KS := Pressed;
        end if;
      when 12 => -- key up
        if Event.keyboard.keycode = 59 then
          InpS.Esc_KS := Unpressed;
        end if;
        if Event.keyboard.keycode = 67 then
          InpS.Start_KS := Unpressed;
        end if;
        if Event.keyboard.keycode = 23 then
          InpS.Mv_Up_KS := Unpressed;
        end if;
        if Event.keyboard.keycode = 19 then
          InpS.Mv_Down_KS := Unpressed;
        end if;
        if Event.keyboard.keycode = 1 then
          InpS.Mv_Left_KS := Unpressed;
        end if;
        if Event.keyboard.keycode = 4 then
          InpS.Mv_Right_KS := Unpressed;
        end if;
      when others =>
        null;
    end case;
  end Update_Input;
  
  procedure Update_State (
    State_In_Out : in out Game_State_Access;
    InpS : in Input_State;
    UpdateDelta : Duration) is
  begin
    case State_In_Out.S is
      when MainMenu =>
        if InpS.Start_KS = Pressed then
          State_In_Out := new Game_State(InLevel);
          State_In_Out.Scn.Append(new Scene_Data'(Ops => Player, ID => 99, PositionInWorld => Pos'(X => 40.0, Y => 40.0)));
        end if;
      when InLevel =>
        if InpS.Mv_Up_KS = Pressed and InpS.Mv_Down_KS = Pressed then
          null;
        elsif InpS.Mv_Up_KS = Pressed and InpS.Mv_Down_KS = Unpressed then
          --PlayerMoveSpeed used to move player, need to calculate based on time since last update for frame-independent movement
          null;
        elsif InpS.Mv_Down_KS = Pressed and InpS.Mv_Up_KS = Unpressed then
          null;
        end if;
        if InpS.Mv_Left_KS = Pressed and InpS.Mv_Right_KS = Pressed then
          null;
        elsif InpS.Mv_Left_KS = Pressed and InpS.Mv_Right_KS = Unpressed then
          null;
        elsif InpS.Mv_Right_KS = Pressed and InpS.Mv_Left_KS = Unpressed then
          null;
        end if;
        
        for E of State_In_Out.Scn loop
          case E.Ops is
            when Player =>
              for D of State_In_Out.Scn loop
                --if E then
                --end if;
                Put_Line("Do player collisions & stuff here");
              end loop;
            when others =>
              null;
          end case;
        end loop;
      when ExitGame =>
        null;
    end case;
    if InpS.Esc_KS = Pressed then
      State_In_Out := new Game_State(ExitGame);
    end if;
  end Update_State;
  
  UpdateInterval : constant Duration := 0.033;
  Frame_Start_Time : Time;
  GS : Game_State_Access;
  Q : access ALLEGRO_EVENT_QUEUE;
  Display : access ALLEGRO_DISPLAY;
  DisplayEventSrc : access ALLEGRO_EVENT_SOURCE;
  KBEventSrc : access ALLEGRO_EVENT_SOURCE;
  Ev : access ALLEGRO_EVENT := new ALLEGRO_EVENT;
  InpState : Input_State;
  PrevUpdateTime : Time := Clock;
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
    GS := new Game_State(MainMenu);
    loop
      Frame_Start_Time := Clock;
      Draw_Game(GS);
      if al_get_next_event(Q, Ev) then
        case Ev.c_type is
          when 42 => -- display close
            GS := new Game_State(ExitGame);
          when others =>
            Update_Input(InpState, Ev);
        end case;
      end if;
      UpdateStateDelta:
        declare
          CurrentUpdateTime : constant Time := Clock;
        begin
          Update_State(GS, InpState, CurrentUpdateTime - PrevUpdateTime);
          PrevUpdateTime := CurrentUpdateTime;
        end UpdateStateDelta;
      exit when GS.S = ExitGame;
      FrameTimeUpdate:
        declare
          FrameTime : constant Duration := Clock - Frame_Start_Time;
        begin
          if FrameTime < UpdateInterval then
            delay UpdateInterval - FrameTime;
          end if;
        end FrameTimeUpdate;
    end loop;
    al_destroy_event_queue(Q);
    al_destroy_display(Display);
  else
    Put_Line("Failure.");
  end if;
end Manpoweroverthrowredoada;
