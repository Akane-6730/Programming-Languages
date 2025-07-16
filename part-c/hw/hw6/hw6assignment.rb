# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # class array holding all the pieces and their rotations
  # 2. Add three special pieces
  All_My_Pieces = All_Pieces + [[[[0, 0], [0, 1], [0, 2], [0, -1], [0, -2]],
                                 [[0, 0], [1, 0], [2, 0], [-1, 0], [-2, 0]]],
                                rotations([[0, 0], [1, 0], [0, 1]]),
                                rotations([[0, 0], [-1, 0], [-1, -1], [0, -1], [1, 0]])]
  Cheat_Piece = [[[0,0]]]

  def self.next_piece(board)
    MyPiece.new(All_My_Pieces.sample, board)
  end

  def self.next_cheat_piece(board)
    MyPiece.new(Cheat_Piece, board)
  end

end

class MyBoard < Board
  # 3. Cheating logic
  def cheat
    return if score < 100 or @cheat_next
    @score -= 100
    @cheat_next = true
  end

  def next_piece
    if @cheat_next
      @current_block = MyPiece.next_cheat_piece(self)
      @cheat_next = false
    else
      @current_block = MyPiece.next_piece(self)
    end
    @current_pos = nil
  end

  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    (0..locations.size - 1).each{|index|
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] =
      @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end


end

class MyTetris < Tetris
  # 1. press 'u' to rotate 180 degrees
  def key_bindings
    super
    @root.bind('u', proc {2.times {@board.rotate_clockwise}})
    @root.bind('c', proc {@board.cheat})
  end

  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)  # Use my board
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

end
