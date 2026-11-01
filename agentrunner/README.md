## Harness for the email assistant

The purpose of the harness is to run agents that will use 3 MCP servers:

- IMAP (to Proton Mail bridge)
- Google Drive (to the documents)
- 

### Setup notes

MCP servers are defined in `.vscode/msp.json`.

Google Drice MCP was set up by creating a project in Google Cloud Console, configuring a personal email as an authorized test user, then downloading a credentials json file, moving the file to path `~/.config/google-drive-mcp/gcp-oauth.keys.json`, and then running `npx -y @piotr-agier/google-drive-mcp auth` and authorizing the MCP server via browser (read-only access was chosen).
