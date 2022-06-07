package dataanalysis;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.javatuples.Pair;

import com.opencsv.exceptions.CsvValidationException;

import datamining.Utility;

public class WalkForward {

    private static final String CSV_VERSIONS = "03-versionsdata.csv";
    private static final String CSV_METHRICS = "04-data.csv";

    public static  List<Pair<List<String>, String>> versionSplit() throws CsvValidationException, IOException{
        List<List<String>> versions = Utility.csvToList(CSV_VERSIONS);
        Integer i;
        Integer j;
        List<Pair<List<String>, String>> iteration = new ArrayList<>();
        for(i=2; i<versions.size(); i++){
            String testing = versions.get(i).get(0);
            List<String> training = new ArrayList<>();
            for(j=1; j<i; j++){
                training.add(versions.get(j).get(0));
            } 
            iteration.add(new Pair<>(training, testing));
        }

        return iteration;
    }

    public static void walkFwIncremental() throws CsvValidationException, IOException{
        Integer i;
        Integer j;
        List<Pair<List<String>, String>> trainingTesting = versionSplit();
        for(i=0; i<trainingTesting.size();i++){
            List<String> training = trainingTesting.get(i).getValue0();
            String testing = trainingTesting.get(i).getValue1();
            System.out.println("Training: "+training+"\nTesting: "+testing);
        }
    }

    public static void main(String[] args) throws CsvValidationException, IOException{
        walkFwIncremental();
    }

}