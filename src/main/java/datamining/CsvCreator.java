package datamining;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.logging.Logger;

import com.opencsv.CSVReader;
import com.opencsv.CSVWriter;
import com.opencsv.exceptions.CsvException;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;


public class CsvCreator {
    private CsvCreator() {
        throw new IllegalStateException("Utility class");
    }

    private static final Logger LOGGER = Logger.getLogger(CsvCreator.class.getName());
    public static final String PRJ_NAME = "SYNCOPE";
    private static final String CSV_COMMIT = "01-commitdata.csv";
    private static final String CSV_JIRA = "02-ticketdata.csv";
    private static final String CSV_VERSIONS = "03-versionsdata.csv";
    private static final String CSV_METHRICS = "04-data.csv";
    public static final boolean DOWNLOAD_DATA = false;
    public static final boolean DOWNLOAD_FILES = false;


    public static void downloadFiles() throws IOException, ParseException{
        LOGGER.warning("Download file per release in corso...");

        Integer k;
        Integer i = 0;
        try(BufferedReader brVd = new BufferedReader(new FileReader(CSV_VERSIONS))){
            String lineVd = brVd.readLine();
            String [] commitSha = new String[0];
            try(FileWriter csvWriter = new FileWriter(CSV_METHRICS)){
                csvWriter.append("versione,file,LOC Touched,LOC added,Max LOC added,Avg LOC added,Churn,Max churn,Avg churn,Change set size,Max change set,Avg change set\n");

                while((lineVd = brVd.readLine()) != null ) {
                    String[] valuesVd = lineVd.split(",");
                    String[] newArray = new String[commitSha.length + 1];
                    System.arraycopy(commitSha, 0, newArray, 0, commitSha.length);
                    commitSha = newArray;

                    commitSha[i] = dateSearch(valuesVd[1]);
                    LOGGER.warning(valuesVd[0]);

                    if(!commitSha[i].equals(" ")){
                        String filesUrl = "https://api.github.com/repos/apache/"+PRJ_NAME+"/git/trees/"+commitSha[i]+"?recursive=1";
                        JSONObject filesJsonObj = Utility.readJsonObjFromUrl(filesUrl, true);
                        JSONArray jsonFiles = new JSONArray(filesJsonObj.getJSONArray("tree"));

                        for(k = 0; k<jsonFiles.length(); k++){
                            if(jsonFiles.getJSONObject(k).getString("path").contains(".java")){
                                csvWriter.append(valuesVd[0]+","+jsonFiles.getJSONObject(k).getString("path")+",0,0,0,0,0,0,0,0,0,0,"+"No\n");
                            }
                        }
                    }
                }
                i++;
            }
        }

    }

    public static void searchBugginess(String affectedVersion, String commitFiles) throws IOException, CsvException{
        Integer j=1;
        List<List<String>> dataCsv = Utility.csvToList(CSV_METHRICS);
        while(j<dataCsv.size() && !dataCsv.get(j).get(0).equals(affectedVersion)){
            j++;
        }
        while(j<dataCsv.size() && dataCsv.get(j).get(0).equals(affectedVersion)){
            if(commitFiles.contains(dataCsv.get(j).get(1))){
                updateDataCSV(CSV_METHRICS, "YES", j, 12);
            }
            j++;
        }
    }


    /**
     * Metodo che ottiene una lista delle versioni del progetto tramite il metodo projectVersions()
     * per ogni versione legge il file ticketdata contenente tutti i commit con le informazioni dei
     * ticket. Per ogni riga chiama il metodo fileTouched() che controlla se la versione corrente
     * rientra nella lista delle affected versions (se presenti), prende lo sha del commit e segna
     * come buggy tutti i file toccati dal commit risolutivo del ticket.
     * 
     * @return void
     * @throws InterruptedException
     */
    public static void bugginess() throws IOException, JSONException, ParseException, CsvException{
        Integer z;
        if(DOWNLOAD_FILES){
            downloadFiles();
        }

        try(BufferedReader brTd = new BufferedReader(new FileReader(CSV_JIRA))){
            String lineTd = brTd.readLine();
            while ( (lineTd = brTd.readLine()) != null ) {
                String[] ticketValues = lineTd.split(",");

                String valuesPure = ticketValues[3].replace("[", "");
                valuesPure = valuesPure.replace("]", "");
                valuesPure = valuesPure.replace("\"", "");
                
                String[] ticketAffectedVer =  valuesPure.split(" ");
                for(z=0; z<ticketAffectedVer.length; z++){
                    if(ticketValues.length > 8) {
                            searchBugginess(ticketAffectedVer[z], ticketValues[8]);
                        } 
                    }
                    }
        }  
    }
    
    /**
    * Update CSV by row and column
    * 
    * @param fileToUpdate CSV file path to update e.g. D:\\chetan\\test.csv
    * @param replace Replacement for your cell value
    * @param row Row for which need to update 
    * @param col Column for which you need to update
    * @throws IOException
    * @throws CsvException
    */
    public static void updateDataCSV(String fileToUpdate, String replace, int row, int col) throws IOException, CsvException {
        List<String[]> csvBody = new ArrayList<>();
        File inputFile = new File(fileToUpdate);

        // Read existing file 
        try(CSVReader reader = new CSVReader(new FileReader(inputFile))){
            csvBody = reader.readAll();
            // get CSV row column  and replace with by using row and column
            csvBody.get(row)[col] = replace;
        }
            // Write to CSV file which is open
        try(CSVWriter writer = new CSVWriter(new FileWriter(inputFile))){
            writer.writeAll(csvBody);
            writer.flush();
        }
    }

    public static String dateSearch(String dateStr) throws ParseException, IOException{
        Date releaseDate = new SimpleDateFormat("yyyy-MM-dd").parse(dateStr);
        File file = new File(CSV_COMMIT);
        Integer i;
        String lastCommitSha = " ";
        try(BufferedReader br = new BufferedReader(new FileReader(file))){
            String line;
            line = br.readLine();
            while ( (line = br.readLine()) != null ) {
                String[] values = line.split(",");
                Date commitDate = new SimpleDateFormat("yyyy-MM-dd").parse(values[0].substring(0, 10));
                i = releaseDate.compareTo(commitDate);
                if(i>=0){
                    lastCommitSha = values[1];
                    break;
                }
            }          
        }
        return lastCommitSha;
    }


    public static void data(String file, boolean incremental) throws IOException, InterruptedException, JSONException, ParseException, CsvException{
        DataRetrieve.fileHandler(file, incremental);
        if(DOWNLOAD_DATA){
            bugginess();
        }
        Methrics.locTouched();
    }
}
