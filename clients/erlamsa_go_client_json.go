package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"log"
	"net/http"

	b64 "encoding/base64"
)

func erlamsa(uri string, data string) string {
	b64input := b64.StdEncoding.EncodeToString([]byte(data))

	message := map[string]interface{}{
		"data": b64input,
		// Add JSON options here, e.g. patterns, mutations, seed
		// "seed": "59737,51792,13772",
	}

	bytesRepresentation, err := json.Marshal(message)
	if err != nil {
		log.Fatalln(err)
	}

	resp, err := http.Post(uri+"/erlamsa/erlamsa_esi:json", "application/json", bytes.NewBuffer(bytesRepresentation))
	if err != nil {
		log.Fatalln(err)
	}

	var result map[string]interface{}

	json.NewDecoder(resp.Body).Decode(&result)

	b64output, ok := result["data"].(string)

	if ok {
		fuzzed, _ := b64.StdEncoding.DecodeString(b64output)
		return string(fuzzed)
	}

	log.Fatalln("Erlamsa returned weird JSON document!")

	return ""
}

func main() {
	var data = "Hello erlamsa from Go!"

	var fuzzeddata = erlamsa("http://localhost:17771", data)

	fmt.Println(data, "erlamsed to", fuzzeddata)
}
