package elisaverza;

import java.io.IOException;
import java.text.ParseException;
import java.util.Arrays;
import java.util.List;

import com.opencsv.exceptions.CsvException;

public class Methrics {

    private static final String CSV_METHRICS = "04-data.csv";
    private static final String CSV_CACHE = "05-commitcache.csv";


    public static void locTouched() throws IOException, ParseException, CsvException, InterruptedException{
        Integer i;
        Integer j;
        Integer k;
        Integer currLocT = 0;
        List<List<String>> commit = DataRetrieve.csvToList(CSV_CACHE);
        List<List<String>> methrics = DataRetrieve.csvToList(CSV_METHRICS);
        for(i=0; i<commit.size(); i++){
            for(j=0; j<methrics.size(); j++){
                if(commit.get(i).get(0).equals(methrics.get(j).get(0))){

                    String[] files = commit.get(i).get(2).split(" ");
                    String[] added = commit.get(i).get(3).split(" ");
                    String[] deleted = commit.get(i).get(4).split(" ");

                    for(k=0; k<files.length; k++){
                        if(files[k].contains(methrics.get(j).get(1))){
                            System.out.println("Sto facendo: "+files[k]+" "+Arrays.toString(added)+" "+Arrays.toString(deleted));
                            currLocT = Integer.valueOf(methrics.get(j).get(2));
                            currLocT = currLocT+Integer.valueOf(added[k])+Integer.valueOf(deleted[k]);
                            CsvCreator.updateDataCSV(CSV_METHRICS, currLocT.toString(), j, 2);

                        }
                    }
                }
            }
        }
    }

    public static void main(String[] args) throws IOException, ParseException, CsvException, InterruptedException{
        locTouched();
    }

}
