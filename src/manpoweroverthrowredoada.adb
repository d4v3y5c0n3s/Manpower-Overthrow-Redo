with allegro5_events_h; use allegro5_events_h;
with allegro5_system_h; use allegro5_system_h;
with allegro5_base_h; use allegro5_base_h;
with allegro5_keyboard_h; use allegro5_keyboard_h;
with allegro_primitives_h; use allegro_primitives_h;
with allegro_image_h; use allegro_image_h;
with Interfaces.C; use Interfaces.C;
with Ada.Text_IO; use Ada.Text_IO;

-- note: current issues were fixed by changing a "limited with X;" to a "with X;"

-- todo: add this to version control

procedure Manpoweroverthrowredoada is
  UpdateInterval : constant Integer := 1/30;
  type Possible_States is (MainMenu, InLevel);
  type Game_State (S : Possible_States) is record
    case S is
      when MainMenu =>
        Test1 : Integer;
      when InLevel =>
        Test2 : Integer;
    end case;
  end record;
  Q : access ALLEGRO_EVENT_QUEUE;
begin
  if al_install_system(Interfaces.C.int(al_get_allegro_version), null) and
  al_install_keyboard and
  al_init_primitives_addon and
  al_init_image_addon then
    Q := al_create_event_queue;
    Put_Line("Success.");
  else
    Put_Line("Failure.");
  end if;
end Manpoweroverthrowredoada;
