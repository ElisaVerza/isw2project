package elisaverza;

import java.io.FileWriter;
import java.io.IOException;
import java.text.ParseException;
import java.util.List;

import com.opencsv.exceptions.CsvException;

public class Methrics {

    private static final String CSV_METHRICS = "04-data.csv";
    private static final String CSV_JIRA = "02-ticketdata.csv";

    private static void resetFile(List<List<String>> file) throws IOException{
        Integer i;
        try(FileWriter csvWriter = new FileWriter(CSV_METHRICS)){
            for(i=1; i<file.size(); i++){
                csvWriter.append(file.get(i).get(0)+","+file.get(i).get(1)+",0,0,0,0,0,0,0,0,0,"+file.get(i).get(11)+"\n");
            }

        }
    }

    public static void methricsWriter(String filesString, String addedString, String deletedString, List<List<String>> methrics, Integer j, Integer temp) throws IOException, CsvException, InterruptedException{
        Integer k;
        Integer currLocT = 0;

        String[] files = filesString.split(" ");
        String[] added = addedString.split(" ");
        String[] deleted = deletedString.split(" ");

        for(k=0; k<files.length; k++){
            if(files[k].equals(methrics.get(j).get(1))){

                currLocT = Integer.valueOf(methrics.get(j).get(2));
                currLocT = currLocT+Integer.valueOf(added[k])+Integer.valueOf(deleted[k]);
                CsvCreator.updateDataCSV(CSV_METHRICS, currLocT.toString(), j, 2);
                //locAdded(added[k], j);

            }
        }
    }

    public static void locTouched() throws IOException, CsvException, NumberFormatException, ParseException, InterruptedException{
        Integer i;
        Integer j;
        List<List<String>> commit = Utility.csvToList(CSV_JIRA);
        List<List<String>> methrics = Utility.csvToList(CSV_METHRICS);
        if(!CsvCreator.DOWNLOAD_DATA){
            resetFile(methrics);
        }
        for(i=1; i<commit.size(); i++){
            j=1;
            while(!commit.get(i).get(0).equals(methrics.get(j).get(0))){
                j++;
            }
            while(j<methrics.size() && commit.get(i).get(0).equals(methrics.get(j).get(0))){
                if(commit.get(i).size()>9){
                    methricsWriter(commit.get(i).get(8), commit.get(i).get(9), commit.get(i).get(10), methrics, j, i);
                }
                j++;
            }
        }
    }

    public static void locAdded(String added, Integer row) throws IOException, CsvException{
        CsvCreator.updateDataCSV(CSV_METHRICS, added, row, 3);
    }

    public static void main(String[] args) throws IOException, CsvException, NumberFormatException, ParseException, InterruptedException{
        locTouched();
    }

}
