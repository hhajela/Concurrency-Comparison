import java.util.ArrayList;


/*
 * 
 * 
 * This class is helper for board class
 * 
 * 
 * */
public class BoardHelper {

	
	public BoardHelper()
	{
		
	}	
	
		//This function return deep copy of the board
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
		
		/*
		 * This function print the row and column for the cell and the potential values for it
		 * 
		 * */
		public void printBoard(Cell[][] board)
		{
			int size = board.length;
			
			for(int i=0;i<size;i++)
				for(int j=0;j<size;j++)
					board[i][j].print();
		}
		
		/*
		 * 
		 *This function print the board which is passed to it 
		 * 
		 */
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
