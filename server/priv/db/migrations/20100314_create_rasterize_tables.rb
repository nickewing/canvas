
# CREATE SEQUENCE tiles_id_seq;
# CREATE TABLE tiles (
#   id              integer PRIMARY KEY DEFAULT nextval('tiles_id_seq'),
#   x               integer NOT NULL,
#   y               integer NOT NULL,
#   last_painted    bigint NOT NULL DEFAULT 0,
#   last_line       bigint NOT NULL DEFAULT 0,
#   painting_status integer NOT NULL DEFAULT 0
# );
# 
# CREATE TABLE line_archive (
#   id            integer PRIMARY KEY,
#   bounding_box  box NOT NULL,
#   points        text NOT NULL,
#   color         integer NOT NULL,
#   size          integer NOT NULL,
#   ip            varchar(15) NOT NULL,
#   time          bigint NOT NULL
# );

class CreateRasterizeTables < Sequel::Migration
  TILE_SIZE = 500.0
  
  def up
    create_table :tiles do
      primary_key :id
      Bignum      :x, :null => false
      Bignum      :y, :null => false
      box         :bounding_box, :null => false
      Bignum      :last_painted, :null => false, :default => 0
      Bignum      :last_line, :null => false, :default => 0
      Integer     :painting_status, :null => false, :default => 0
      
      index [:x, :y]
    end
    
    create_table :line_archive do
      primary_key :id
      box         :bounding_box, :null => false
      text        :points, :null => false
      Integer     :color, :null => false
      Integer     :size, :null => false
      String      :ip, :size => 15, :null => false
      Bignum      :time, :null => false
    end
    
    alter_table :lines do
      set_column_allow_null :bounding_box, false
      set_column_allow_null :points, false
      set_column_allow_null :color, false
      set_column_allow_null :size, false
      set_column_allow_null :ip, false
      set_column_allow_null :time, false
    end
    
    DB[:lines].each do |l|
      box   = db_str_to_box(l[:bounding_box])
      tiles = tiles_in_box(box)
      
      tiles.each do |t|
        tile_box = [
          t[0],
          t[1],
          t[0] + TILE_SIZE,
          t[1] + TILE_SIZE
        ]
        xy = {:x => t[0], :y => t[1]}
        vals = {
          :last_line    => l[:time],
          :bounding_box => box_to_db_str(tile_box)
        }
        
        if tile = DB[:tiles].where(xy).first
          DB[:tiles].where(:id => tile[:id]).update(vals)
        else
          DB[:tiles].insert(xy.merge(vals))
        end
      end
    end
    
  end
  def down
    drop_table :tiles
    drop_table :line_archive
  end
  
  private
  
  def tiles_in_box(box)
    t0 = [tile_coord(box[0]), tile_coord(box[1])]
    tf = [tile_coord(box[2]), tile_coord(box[3])]
    
    x_dir = tf[0] == t0[0] ? 1.0 : (tf[0] - t0[0]) / (tf[0] - t0[0]).abs
    y_dir = tf[1] == t0[1] ? 1.0 : (tf[1] - t0[1]) / (tf[1] - t0[1]).abs
    
    x_steps = ((tf[0] - t0[0]).abs / TILE_SIZE).ceil.to_i + 1
    y_steps = ((tf[1] - t0[1]).abs / TILE_SIZE).ceil.to_i + 1
    
    tiles = []
    x_steps.times do |i|
      y_steps.times do |j|
        tiles << [
          (t0[0] + x_dir * i * TILE_SIZE).to_i,
          (t0[1] + y_dir * j * TILE_SIZE).to_i
        ]
      end
    end
    
    tiles
  end
  
  def tile_coord(coord)
    (coord / TILE_SIZE).floor * TILE_SIZE
  end
  
  def db_str_to_box(str)
    str =~ /\(([-0-9\.]*),([-0-9\.]*)\),\(([-0-9\.]*),([-0-9\.]*)\)/
    
    [$1.to_f, $2.to_f, $3.to_f, $4.to_f]
  end
  
  def box_to_db_str(box)
    "((#{box[0]},#{box[1]}),(#{box[2]},#{box[3]}))"
  end
end