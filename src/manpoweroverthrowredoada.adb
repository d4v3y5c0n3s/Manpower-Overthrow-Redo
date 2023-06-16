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
  
  type Scalar is digits 9 range -9000.0 .. 9000.0;
  
  type Pos is record
    X : Scalar;
    Y : Scalar;
  end record;

  type Player_Data is record
    PositionInWorld : Pos;
    Acceleration : Pos;
    Velocity : Pos;
  end record;
  
  package PlayersInWorld is new Ada.Containers.Vectors(
    Index_Type => Natural,
    Element_Type => Player_Data
  );
  
  type Enemy_Data is record
    PositionInWorld : Pos;
  end record;
  
  package EnemiesInWorld is new Ada.Containers.Vectors(
    Index_Type => Natural,
    Element_Type => Enemy_Data
  );
  
  type Tile_Data is (None, Dirt);
  
  type Tiles_Array is array(Positive range <>, Positive range <>) of Tile_Data;
  
  type Tile_Grid(Width : Positive; Height : Positive) is record
    Tiles : Tiles_Array(1 .. Width, 1 .. Height) := (Others => (Others => None));
    GridOffset : Pos := Pos'(X => 0.0, Y => 0.0);
    TileWidth : Positive := 32;
    TileHeight : Positive := 32;
  end record;
  
  type Possible_States is (MainMenu, InLevel, ExitGame);
  
  type Game_State (S : Possible_States) is record
    F : access ALLEGRO_FONT := al_create_builtin_font;
    case S is
      when MainMenu =>
        null;
      when InLevel =>
        ScreenPos : Pos := Pos'(X => 0.0, Y => 0.0);
        PIW : PlayersInWorld.Vector;
        EIW : EnemiesInWorld.Vector;
        TG : Tile_Grid(100, 100);
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
  
  MaxPlayerMoveSpeed : constant Scalar := 40.0;
  DefaultPlayerDeceleration : constant Scalar := 6.0;
  DefaultPlayerAcceleration : constant Scalar := 2.0;
  
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
        al_clear_to_color(al_map_rgb(46, 52, 54));
        TilesDraw:
          declare
            Tile_TL : Pos;
            Tile_BR : Pos;
          begin
            for W in State_In.TG.Tiles'Range(1) loop
              for H in State_In.TG.Tiles'Range(2) loop
                Tile_TL := State_In.TG.GridOffset;
                Tile_TL := transform_by(Tile_TL, Pos'(X => Scalar(((W - 1) mod State_In.TG.Width) * State_In.TG.TileWidth), Y => Scalar(((H - 1) mod State_In.TG.Height) * State_In.TG.TileHeight)));
                Tile_BR := transform_by(Tile_TL, Pos'(X => Scalar(State_In.TG.TileWidth), Y => Scalar(State_In.TG.TileHeight)));
                case State_In.TG.Tiles(W, H) is
                  when None =>
                    null;
                  when Dirt =>
                    al_draw_filled_rectangle(Float(Tile_TL.X), Float(Tile_TL.Y), Float(Tile_BR.X), Float(Tile_BR.Y), al_map_rgb(193, 125, 17));
                end case;
              end loop;
            end loop;
          end TilesDraw;
        for P of State_In.PIW loop
          PlayerDraw:
            declare
              TL : Pos := P.PositionInWorld;
              BR : Pos;
            begin
              TL := transform_by(TL, State_In.ScreenPos);
              BR := transform_by(TL, Pos'(X => 64.0, Y => 64.0));
              al_draw_filled_rectangle(Float(TL.X), Float(TL.Y), Float(BR.X), Float(BR.Y), al_map_rgb(138, 226, 52));
          end PlayerDraw;
        end loop;
        for E of State_In.EIW loop
          EnemyDraw:
            declare
              TL : Pos := E.PositionInWorld;
              BR : Pos;
            begin
              TL := transform_by(TL, State_In.ScreenPos);
              BR := transform_by(TL, Pos'(X => 64.0, Y => 64.0));
              al_draw_filled_rectangle(Float(TL.X), Float(TL.Y), Float(BR.X), Float(BR.Y), al_map_rgb(239, 41, 41));
          end EnemyDraw;
        end loop;
        al_draw_text(State_In.F, al_map_rgb(54, 101, 164), 0.0, 0.0, 0, New_String("Yo, it's a level"));
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
          State_In_Out.PIW.Append(Player_Data'(
            PositionInWorld => Pos'(X => 40.0, Y => 40.0),
            Acceleration => Pos'(X => 0.0, Y => 0.0),
            Velocity => Pos'(X => 0.0, Y => 0.0)
          ));
          State_In_Out.EIW.Append(Enemy_Data'(PositionInWorld => Pos'(X => 100.0, Y => 100.0)));
          State_In_Out.TG.Tiles(3, 3) := Dirt;
        end if;
      when InLevel =>
        for P of State_In_Out.PIW loop
          if InpS.Mv_Up_KS = Pressed and InpS.Mv_Down_KS = Pressed then
            P.acceleration.Y := 0.0;
          elsif InpS.Mv_Up_KS = Pressed and InpS.Mv_Down_KS = Unpressed then
            P.acceleration.Y := -DefaultPlayerAcceleration;
            P.velocity.Y := P.velocity.Y + (Scalar(UpdateDelta) * (P.acceleration.Y * MaxPlayerMoveSpeed));
          elsif InpS.Mv_Down_KS = Pressed and InpS.Mv_Up_KS = Unpressed then
            P.acceleration.Y := DefaultPlayerAcceleration;
            P.velocity.Y := P.velocity.Y + (Scalar(UpdateDelta) * (P.acceleration.Y * MaxPlayerMoveSpeed));
          else
            P.velocity.Y := P.velocity.Y + ((-P.velocity.Y) * DefaultPlayerDeceleration) * Scalar(UpdateDelta);
          end if;
          if InpS.Mv_Left_KS = Pressed and InpS.Mv_Right_KS = Pressed then
            P.acceleration.X := 0.0;
            P.velocity.X := P.velocity.X * 0.1;
          elsif InpS.Mv_Left_KS = Pressed and InpS.Mv_Right_KS = Unpressed then
            P.acceleration.X := -DefaultPlayerAcceleration;
            P.velocity.X := P.velocity.X + (Scalar(UpdateDelta) * (P.acceleration.X * MaxPlayerMoveSpeed));
          elsif InpS.Mv_Right_KS = Pressed and InpS.Mv_Left_KS = Unpressed then
            P.acceleration.X := DefaultPlayerAcceleration;
            P.velocity.X := P.velocity.X + (Scalar(UpdateDelta) * (P.acceleration.X * MaxPlayerMoveSpeed));
          else
            P.velocity.X := P.velocity.X + ((-P.velocity.X) * DefaultPlayerDeceleration) * Scalar(UpdateDelta);
          end if;
            P.PositionInWorld.X := P.PositionInWorld.X + (Scalar(UpdateDelta) * P.velocity.X);
            P.PositionInWorld.Y := P.PositionInWorld.Y + (Scalar(UpdateDelta) * P.velocity.Y);
        end loop;
        for E of State_In_Out.EIW loop
          null;
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
