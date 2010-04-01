# -- Database structure
# 
# CREATE SEQUENCE lines_id_seq;
# CREATE TABLE lines (
#   id            integer PRIMARY KEY DEFAULT nextval('lines_id_seq'),
#   bounding_box  box,
#   points        text,
#   color         integer,
#   size          integer,
#   ip            varchar(15),
#   time          bigint
# );


class InitSchema < Sequel::Migration
  def up
    return if DB[:lines]
    create_table :lines do
      primary_key :id
      box         :bounding_box
      text        :points
      Integer     :color
      Integer     :size
      String      :ip, :size => 15
      Bignum      :time
    end
  end
  def down
    drop_table :lines
  end
end