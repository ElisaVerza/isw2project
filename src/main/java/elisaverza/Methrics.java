package elisaverza;

import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import com.opencsv.CSVWriter;
import com.opencsv.exceptions.CsvException;

public class Methrics {

    private static final String CSV_METHRICS = "04-data.csv";
    private static final String CSV_JIRA = "02-ticketdata.csv";

    private static void resetFile(List<List<String>> file) throws IOException{
        Integer i;
        try(FileWriter csvWriter = new FileWriter(CSV_METHRICS)){
            csvWriter.append("versione,file,LOC Touched, metrica2, metrica3, metrica4, metrica5, metrica6, metrica7, metrica8, metrica9, bugginess\n");
            for(i=1; i<file.size(); i++){
                csvWriter.append(file.get(i).get(0)+","+file.get(i).get(1)+",0,0,0,0,0,0,0,0,0,"+file.get(i).get(11)+"\n");
            }

        }
    }

    public static void methricsWriter(String filesString, String addedString, String deletedString, List<List<String>> methrics, Integer j) throws IOException, CsvException{
        Integer k;
        Integer currLocT;
        String[] files = filesString.split(" ");
        String[] added = addedString.split(" ");
        String[] deleted = deletedString.split(" ");

        for(k=0; k<files.length; k++){
            if(files[k].equals(methrics.get(j).get(1))){
                currLocT = Integer.valueOf(methrics.get(j).get(2));
                currLocT = currLocT+Integer.valueOf(added[k])+Integer.valueOf(deleted[k]);
                Integer counter = Integer.valueOf(methrics.get(j).get(5))+1;
                methrics.get(j).set(2, currLocT.toString());
                methrics.get(j).set(5, counter.toString());
                locAdded(methrics, added[k], j);
                maxLocAdded(methrics, added[k], j);
            }
        }
    }

    public static void locTouched() throws IOException, CsvException, NumberFormatException{
        Integer i;
        Integer j;
        List<List<String>> commit = Utility.csvToList(CSV_JIRA);
        List<List<String>> methrics = Utility.csvToList(CSV_METHRICS);
        if(!CsvCreator.DOWNLOAD_DATA){
            resetFile(methrics);
            methrics = Utility.csvToList(CSV_METHRICS);
        }

        for(i=1; i<commit.size(); i++){
            j=1;
            while(!commit.get(i).get(0).equals(methrics.get(j).get(0))){
                j++;
            }
            while(j<methrics.size() && commit.get(i).get(0).equals(methrics.get(j).get(0))){
                if(commit.get(i).size()>9){
                    methricsWriter(commit.get(i).get(8), commit.get(i).get(9), commit.get(i).get(10), methrics, j);
                }
                j++;
            }
        }
        try(CSVWriter csvWriter = new CSVWriter(new FileWriter(CSV_METHRICS));){
            List<String[]> collect = new ArrayList<>();
            for(i=0; i<methrics.size();i++){
                String[] strArray = methrics.get(i).toArray(String[]::new);
                collect.add(strArray);
            }
            csvWriter.writeAll(collect);        
        }
    }

    public static void locAdded(List<List<String>> methrics, String added, Integer row) throws IOException, CsvException{
        Integer currLocA = Integer.valueOf(methrics.get(row).get(3));
        currLocA = currLocA+Integer.valueOf(added);
        methrics.get(row).set(3, currLocA.toString());
    }

    public static void maxLocAdded(List<List<String>> methrics, String currAdded, Integer row) throws IOException, CsvException{
        Integer lastLocA = Integer.valueOf(methrics.get(row).get(4));
        if(lastLocA<Integer.valueOf(currAdded)){
            methrics.get(row).set(4, currAdded);
        }
    }

    public static void avgLocAdded(){

    }

    public static void main(String[] args) throws IOException, CsvException, NumberFormatException{
        locTouched();
    }

}
