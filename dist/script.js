const init = async () => {
  const db = await getDb()

  // Check if table exists
  db.transaction('tables', 'readwrite').objectStore('tables').getAll().onsuccess = e => {
    const tables = e.target.result

    const tableNames = [ 'boms', 'items' ]
    tableNames.forEach(tableName => {
      if (!tables.find(table => table.name === tableName)) {
        db.transaction('tables', 'readwrite').objectStore('tables').add({ name: tableName, contents: [] })
      }
    })
  }

  var app = Elm.Main.init({
    node: document.getElementById('myapp'), 
    flags: Math.floor(Math.random()*0x0FFFFFFF)
  })

  app.ports.idbGet.subscribe(tableName => {
    db.transaction('tables', 'readwrite').objectStore('tables').getAll().onsuccess = e => {
      const foundTable = e.target.result.find(table => table.name === tableName)

      console.log("Found table:", foundTable)

      if (foundTable) {
        app.ports.idbRecv.send({
          name: tableName,
          contents: JSON.stringify(foundTable.contents)
        }) 
      }
    }
  })

  app.ports.idbAdd.subscribe(recvModel => {
    console.log("Recv model:", recvModel, JSON.parse(recvModel.contents))
    db.transaction('tables', 'readwrite').objectStore('tables').getAll().onsuccess = e => {
      const foundTable = e.target.result.find(table => table.name === recvModel.name)
      
      console.log('Found table:', foundTable)

      if (foundTable) {
        foundTable.contents = [ ...foundTable.contents, JSON.parse(recvModel.contents) ]

        db.transaction('tables', 'readwrite').objectStore('tables').put(foundTable).onsuccess = e => {
          db.transaction('tables', 'readwrite').objectStore('tables').getAll().onsuccess = e => {
            app.ports.idbRecv.send({
              name: recvModel.name,
              contents: JSON.stringify(foundTable.contents)
            }) 
          }
        }
      }
    }
  })

  app.ports.idbDelete.subscribe(deleteModel => {
    db.transaction('tables', 'readwrite').objectStore('tables').getAll().onsuccess = e => {
      const foundTable = e.target.result.find(table => table.name === deleteModel.name)

      if (foundTable) {
        foundTable.contents.splice(
          foundTable.contents.findIndex(contentToFind => contentToFind.uuid === deleteModel.uuid),
          1
        )

        db.transaction('tables', 'readwrite').objectStore('tables').put(foundTable).onsuccess = e => {
          db.transaction('tables', 'readwrite').objectStore('tables').getAll().onsuccess = e => {
            app.ports.idbRecv.send({
              name: deleteModel.name,
              contents: JSON.stringify(foundTable.contents)
            }) 
          }
        }
      }
    }
  })
}

const getDb = () => {
  return new Promise((resolve, reject) => {
    const request = indexedDB.open('vmasdanisummer')

    request.onerror = e => {
      reject(e)
    } 

    request.onsuccess = e => {
      resolve(e.target.result)
    }

    request.onupgradeneeded = e => {
      e.target.result.createObjectStore('tables', { keyPath: 'name', unique: true })
    }
  })
}

init()