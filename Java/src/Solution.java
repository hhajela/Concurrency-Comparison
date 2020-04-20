
public class Solution {

	public static void main(String[] args) {
		
	int[][]	board = {
			  {5,3,0,0,7,0,0,0,0},
			  {6,0,0,1,9,5,0,0,0},
			  {0,9,8,0,0,0,0,6,0},
			  {8,0,0,0,6,0,0,0,3},
			  {4,0,0,8,0,3,0,0,1},
			  {7,0,0,0,2,0,0,0,6},
			  {0,6,0,0,0,0,2,8,0},
			  {0,0,0,4,1,9,0,0,5},
			  {0,0,0,0,8,0,0,7,9}
			};
	SodukoSolver sk = new SodukoSolver(board,9);
	Cell[][] local = new Cell[9][9];
	local = sk.getboard();
	local = sk.getCopy(local);
	Cell[][] sol = sk.solve(local);
	System.out.println("Soduko Solved");
	for(int i=0;i<9;i++)
		{for(int j=0;j<9;j++)
			{
			System.out.print(sol[i][j].getIntValue()+" ");
			}
			System.out.print("\n");
		}
	}
	

}
