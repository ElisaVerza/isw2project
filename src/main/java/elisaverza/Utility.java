package elisaverza;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;
import java.net.URLConnection;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Base64;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import com.opencsv.CSVReader;
import com.opencsv.exceptions.CsvValidationException;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

public class Utility {

    private static final String CSV_VERSIONS = "03-versionsdata.csv";
    private static final String PRJ_NAME = "BOOKKEEPER";
    private static final String USERNAME = "ElisaVerza";
    private static final String AUTH_CODE = "auth_code.txt";
    private static final String DATE_FORMAT = "yyyy-MM-dd";

//Salta
    public static Date getLastRelease() throws IOException, ParseException{
        Date last = new Date();
        try(BufferedReader br = new BufferedReader(new FileReader(CSV_VERSIONS))){
            String line = br.readLine();
            while ( (line = br.readLine()) != null ) {
                String[] values = line.split(",");
                last = new SimpleDateFormat(DATE_FORMAT).parse(values[1]);
            }
        }
        return last;
    }
//Salta
    public static void sortVersions() throws IOException{
        Integer i;
        Map<String, String> versionMap = new HashMap<>();
        try(BufferedReader br = new BufferedReader(new FileReader(CSV_VERSIONS))){
            String line = br.readLine();
            while ( (line = br.readLine()) != null ) {
                String[] values = line.split(",");
                versionMap.put(values[1], values[0]);
            }
        }
        Map<String, String> sortedMap = new TreeMap<>(versionMap);
        List<String> date = new ArrayList<>(sortedMap.keySet());
        List<String> version = new ArrayList<>(sortedMap.values());
        File versionsFile = new File(CSV_VERSIONS);
        try(FileWriter versionsWriter = new FileWriter(versionsFile)){
            versionsWriter.append("name, release date\n");
            for(i=0; i<version.size();i++){
                versionsWriter.append(version.get(i)+","+date.get(i)+"\n");
            }
        }
    }
//Salta

    public static String getVersionByDate(String date) throws IOException, ParseException{
        Date openingToDate = new SimpleDateFormat(DATE_FORMAT).parse(date);
        Date versionToDate = new Date();
        String version = " ";
        try(BufferedReader br = new BufferedReader(new FileReader(CSV_VERSIONS))){
            String line = br.readLine();
            while ( (line = br.readLine()) != null ) {
                String[] values = line.split(",");
                versionToDate = new SimpleDateFormat(DATE_FORMAT).parse(values[1]);
                if(versionToDate.compareTo(openingToDate)>=0){
                    version = values[0];
                    break;
                }
            }
        }
        return version;
    }

    /**
     * Metodo che permette di autenticare le richieste fatte a github per evitare 
     * il limit rate sulle chiamate get.
     * 
     * @param url indirizzo della richiesta
     * @return input stream reader
     */

    public static InputStreamReader auth(URL url) throws IOException{
        String token;
        URLConnection uc = url.openConnection();
        uc.setRequestProperty("X-Requested-With", "Curl");

        try(BufferedReader oauthReader = new BufferedReader(new FileReader(AUTH_CODE));){
	 	   token = oauthReader.readLine();
	    }
        String username =  USERNAME;
        String userpass = username + ":" + token;
        byte[] encodedBytes = Base64.getEncoder().encode(userpass.getBytes());
        String basicAuth = "Basic " + new String(encodedBytes);
        uc.setRequestProperty("Authorization", basicAuth);

        return new InputStreamReader(uc.getInputStream());
    }

    /**
     * Metodo che prende in input un buffered reader e ne restituisce il contenuto
     * in formato stringa.
     *  
     * @param rd buffered reader da cui leggere
     * @return contenuto del buffered reader il formato stringa
     */
    private static String readAll(BufferedReader rd) throws IOException {
        StringBuilder sb = new StringBuilder();
        int cp;
        while ((cp = rd.read()) != -1) {
           sb.append((char) cp);
        }
        return sb.toString();
     }

     /**
     * Metodo per eseguire una get dall'url specificato dal parametro omonimo da cui 
     * si otterrà un JsonArray. Se la richiesta è inidirizzata a github viene chiamato 
     * il metodo auth che provvederà all'autenticazione della richiesta per evitare la 
     * limitazione delle richieste.
     * 
     * @param url URL da cui effetturare la get
     * @param git booleano per riconoscere se la get è indirizzata a github o no
     * @return jsonText JsonArray ottenuto con la chiamata get
     */
     public static JSONArray readJsonArrayFromUrl(String url, boolean git) throws IOException, JSONException {
        InputStreamReader is;
        if(git){
            URL strToUrl = new URL(url);
            is = auth(strToUrl);
        }
        else{
            InputStream iStream = new URL(url).openStream();
            is = new InputStreamReader(iStream);
        }
        try(BufferedReader rd = new BufferedReader(is)) {
           String jsonText = readAll(rd);

           return new JSONArray(jsonText);
         } finally {
           is.close();
         }
     }

     /**
     * Metodo per eseguire una get dall'url specificato dal parametro omonimo da cui 
     * si otterrà un JsonObject. Se la richiesta è inidirizzata a github viene chiamato 
     * il metodo auth che provvederà all'autenticazione della richiesta per evitare la 
     * limitazione delle richieste.
     * 
     * @param url URL da cui effetturare la get
     * @param git booleano per riconoscere se la get è indirizzata a github o no
     * @return jsonText JsonObject ottenuto con la chiamata get
     */
    public static JSONObject readJsonObjFromUrl(String url, boolean git) throws IOException, JSONException {
        InputStreamReader is;
        if(git){
            URL strToUrl = new URL(url);
            is = auth(strToUrl);
        }
        else{
            InputStream iStream = new URL(url).openStream();
            is = new InputStreamReader(iStream);
        }
        try(BufferedReader rd = new BufferedReader(is)){
           String jsonText = readAll(rd);
           return new JSONObject(jsonText);
         } finally {
           is.close();
         }
     }

     /**
      * Metodo per cercare il jira id nei commenti dei commit.
      *
      * @param toParse stringa del messaggio di commit in cui cercare il jira id
      * @return parseId stringa contenente il jira id
      */
    public static String parseId(String toParse) throws SecurityException{
        StringBuilder parsed = new StringBuilder();        
        Integer j = 0;

        if(toParse.contains(PRJ_NAME)){
            j = toParse.indexOf(PRJ_NAME);
            j=j+PRJ_NAME.length()+1;
            parsed.append(PRJ_NAME+"-");

            while(toParse.length()>j){
                if(toParse.length()>j && Character.isDigit(toParse.charAt(j))){
                    parsed.append(toParse.charAt(j));
                }
                else{break;}
                j++;
            }
        }
        return parsed.toString();
    }
    
    /**
     * Metodo cercare un determinato valore in una colonna del csv. Il csv viene letto riga per riga
     * ed ad ognuna di queste viene fatto il parsing per colonna. Ogni riga viene confrontato il 
     * contenuto della colonna specificata con la stringa che si sta cercando, alla fine del file
     * viene restituito un array con il contenuto di tutte le righe contenenti la stringa cercata.
     * 
     * @param searchColumnIndex indice della colonna del csv (0 bound)
     * @param searchString stringa da cercare
     * @param file nome del file csv in cui cercare
     * @return resultRow array di stringhe contenente le righe che contengono searchString
     */
    public static String[] searchCsvLine(int searchColumnIndex, String searchString, String file) throws IOException {
        String[] resultRow = new String[0];
        Integer i = 0;
        try(BufferedReader br = new BufferedReader(new FileReader(file))){
            String line;
            while ( (line = br.readLine()) != null ) {
                String[] values = line.split(",");

                if(values.length > searchColumnIndex && values[searchColumnIndex].equals(searchString)) {
                    String[] newArray = new String[resultRow.length + 1];
                    System.arraycopy(resultRow, 0, newArray, 0, resultRow.length);

                    resultRow = newArray;
                    resultRow[i] = line;
                    i++;
                }
            }
        }

        return resultRow;
    }

    public static List<List<String>> csvToList(String csvFile) throws IOException, CsvValidationException{
        List<List<String>> records = new ArrayList<>();
        try (CSVReader csvReader = new CSVReader(new FileReader(csvFile));) {
            String[] values = null;
            while ((values = csvReader.readNext()) != null) {
                records.add(Arrays.asList(values));
            }
        }  
      return records;
    }

    public static void main(String[] args){
        //not needed
    }
}
