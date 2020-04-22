import java.util.ArrayList;

public class BoardHelper {

	
	public BoardHelper()
	{
		
	}	
		@SuppressWarnings("unchecked")
		public Cell[][] getCopy(Cell[][] board)
		{
			int size = board.length;
			Cell[][] local = new Cell[size][size];
			
			for(int i=0;i<size;i++)
				for(int j=0;j<size;j++)
					{
					local[i][j]= new Cell(board[i][j].getRow(),board[i][j].getCol());
					local[i][j].setPV((ArrayList<Integer>) board[i][j].getPV().clone());
					if(board[i][j].isSet())
						local[i][j].setValue();
					local[i][j].setIntValue(board[i][j].getIntValue());
					}
			
			return local;
			
		}
		
		public void printBoard(Cell[][] board)
		{
			int size = board.length;
			
			for(int i=0;i<size;i++)
				for(int j=0;j<size;j++)
					board[i][j].print();
		}
		
		public void printValueBoard(Cell[][] board)
		{
			int size = board.length;
			for(int i=0;i<size;i++)
			{for(int j=0;j<size;j++)
				{
				System.out.print(board[i][j].getIntValue()+" ");
				}
				System.out.print("\n");
			}
		}
	
	
}
