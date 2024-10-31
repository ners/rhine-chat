let Login =
      < Credentials :
          { username : Text, password : Text, deviceName : Optional Text }
      | SessionToken : Text
      >

in  { baseUrl = "http://localhost:8008"
    , room = "#lounge:synapse.test"
    , login =
        Login.Credentials
          { username = "bot"
          , password = "bot123"
          , deviceName = Some "slothbot"
          }
    }
