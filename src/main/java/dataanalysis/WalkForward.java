package dataanalysis;
import java.io.IOException;
import java.util.List;

import com.opencsv.exceptions.CsvValidationException;

import datamining.Utility;
import datamining.CsvCreator;

public class WalkForward {

    private static final String CSV_VERSIONS = "03-versionsdata.csv";
    private static final String CSV_METHRICS = "04-data.csv";

    public static void versionSplit() throws CsvValidationException, IOException{
        //List<List<String>> versions = Utility.csvToList(CSV_VERSIONS);
        //List<List<String>> data = Utility.csvToList(CSV_METHRICS);
        CsvCreator.data();


    }

    public static void main(String[] args) throws CsvValidationException, IOException{
        versionSplit();
    }

}