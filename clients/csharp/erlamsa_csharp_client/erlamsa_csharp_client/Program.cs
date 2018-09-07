using System;
using System.Net.Http;
using System.Threading.Tasks;
using System.Text;


namespace erlamsa_csharp_client
{
    class Program
    {
        static void Main(string[] args)
        {
            HttpClient httpclient = new HttpClient();

            string erlamsaURL = "http://127.0.0.1:17771";
            string originalString = "Hello erlamsa!";

            UTF8Encoding encoding = new UTF8Encoding();
            byte[] byteData = encoding.GetBytes(originalString);
            HttpContent content = new ByteArrayContent(byteData);

            HttpResponseMessage response = httpclient.PostAsync(erlamsaURL + "/erlamsa/erlamsa_esi:fuzz", content).Result;

            var fuzzedString = response.Content.ReadAsStringAsync().Result;

            Console.WriteLine(originalString + " erlamsed to " + fuzzedString);

            Console.ReadKey();
        }
    }
}
