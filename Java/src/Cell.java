import java.util.*;
/*
 * 
 *This is the class which is representing single cell of soduko.
 *variable row and column stores the index of the cell.
 *PotentialValue is a list of all the potential value for a cell
 *intValue is the current value for the cell
 *value is a boolean which is set when you find the solution for the cell
 *
 * */
public class Cell {
 private int row;
 private int col;
 private int intValue;
 private boolean value = false;
 ArrayList<Integer> potentialValues;
 
 	public Cell(int _row, int _col)
 	{
 		row=_row;
 		col=_col;
 		intValue=0;
 		potentialValues = new ArrayList();
 	}
 	
 	//Method to set the value in the cell
 	public void setIntValue(int v)
 	{
 		this.intValue = v;
 	}
 	
 	
 	//Return the current set value of the cell
 	public int getIntValue()
 	{
 		return this.intValue;
 	}
 	
 	
 	//Return the list of potential values
 	public ArrayList<Integer> getPV()
 	{
 		return potentialValues;
 	}
 	
 	
 	//set the potential value to the sent parameter
 	public void setPV(ArrayList<Integer> p)
 	{
 		this.potentialValues = p;
 	}

 	//Add a value to potential value list
	public void add(int v)
 	{
 		potentialValues.add(v);
 	}
	//Remove a values from potential value list
 	public void remove(int v)
 	{
 		potentialValues.remove(new Integer(v));
 	}
 	
 	//print cell row and column index and potential values
 	public void print()
 	{
 		System.out.print(row+" "+col+": ");
 		for(int i=0;i<potentialValues.size();i++)
 			System.out.print(potentialValues.get(i)+" ");
 		System.out.print("\n");
 	}
 	
 	//Check if the solution for the cell is set
 	public boolean isSet()
 	{
 		return value;
 	}
 	
 	
 	//set the boolean to indicate solution is found for the cell
 	public void setValue()
 	{
 		value=true;
 	}
 	
 	//Return the size of list of potential value
 	public int getPotentialValuesSize()
 	{
 		return potentialValues.size();
 	}
 	
 	
 	//Return the row index
 	public int getRow()
 	{
 		return row;
 	}
 	
 	//return column index
 	public int getCol()
 	{
 		return col;
 	}
 	
 	
 	//Return value from potential list based index parameter
 	public int getPotentialValue(int i)
 	{
 		return this.potentialValues.get(i);
 	}
}
