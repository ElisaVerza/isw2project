package elisaverza;

import java.io.FileWriter;
import java.io.IOException;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.List;

import com.opencsv.exceptions.CsvException;

public class Methrics {

    private static final String CSV_METHRICS = "04-data.csv";
    private static final String CSV_CACHE = "05-commitcache.csv";

    private static void resetFile(List<List<String>> file) throws IOException, CsvException{
        Integer i;
        try(FileWriter csvWriter = new FileWriter(CSV_METHRICS)){
            for(i=1; i<file.size(); i++){
                csvWriter.append(file.get(i).get(0)+","+file.get(i).get(1)+","+0+","+0+","+0+","+0+","+0+","+0+","+0+","+0+","+0+","+file.get(i).get(11));
            }

        }
    }

    public static void locTouched() throws IOException, ParseException, CsvException, InterruptedException{
        Integer i;
        Integer j;
        Integer k;
        Integer currLocT = 0;
        List<List<String>> commit = DataRetrieve.csvToList(CSV_CACHE);
        List<List<String>> methrics = DataRetrieve.csvToList(CSV_METHRICS);
        if(!CsvCreator.DOWNLOAD_DATA){
            resetFile(commit);
        }
        for(i=0; i<commit.size(); i++){
            for(j=0; j<methrics.size(); j++){
                if(commit.get(i).get(0).equals(methrics.get(j).get(0))){

                    String[] files = commit.get(i).get(2).split(" ");
                    String[] added = commit.get(i).get(3).split(" ");
                    String[] deleted = commit.get(i).get(4).split(" ");

                    for(k=0; k<files.length; k++){
                        if(files[k].contains(methrics.get(j).get(1))){
                            currLocT = Integer.valueOf(methrics.get(j).get(2));
                            currLocT = currLocT+Integer.valueOf(added[k])+Integer.valueOf(deleted[k]);
                            CsvCreator.updateDataCSV(CSV_METHRICS, currLocT.toString(), j, 2);
                            locAdded(added[k], j);
                        }
                    }
                }
            }
        }
    }

    public static void locAdded(String added, Integer row) throws IOException, CsvException{
        CsvCreator.updateDataCSV(CSV_METHRICS, added, row, 3);
    }

    public static void main(String[] args) throws IOException, ParseException, CsvException, InterruptedException{
        locTouched();
    }

}
